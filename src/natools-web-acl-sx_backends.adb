------------------------------------------------------------------------------
-- Copyright (c) 2017-2019, Natacha Porté                                   --
--                                                                          --
-- Permission to use, copy, modify, and distribute this software for any    --
-- purpose with or without fee is hereby granted, provided that the above   --
-- copyright notice and this permission notice appear in all copies.        --
--                                                                          --
-- THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES --
-- WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF         --
-- MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR  --
-- ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES   --
-- WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN    --
-- ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF  --
-- OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.           --
------------------------------------------------------------------------------

with Natools.S_Expressions.Atom_Ref_Constructors;
with Natools.S_Expressions.File_Readers;
with Natools.S_Expressions.Interpreter_Loop;

package body Natools.Web.ACL.Sx_Backends is

   type User_Builder (Hash_Id_First, Hash_Id_Last : Hash_Id) is record
      Tokens, Groups : Containers.Unsafe_Atom_Lists.List;
   end record;

   type Backend_Builder (Hash_Id_First, Hash_Id_Last : Hash_Id) is record
      Map : Token_Maps.Unsafe_Maps.Map;
   end record;

   Cookie_Name : constant String := "User-Token";
   Hash_Mark : constant Character := '$';

   procedure Process_User
     (Builder : in out Backend_Builder;
      Context : in Meaningless_Type;
      Name : in S_Expressions.Atom;
      Arguments : in out Natools.S_Expressions.Lockable.Descriptor'Class);

   procedure Process_User_Element
     (Builder : in out User_Builder;
      Context : in Meaningless_Type;
      Name : in S_Expressions.Atom;
      Arguments : in out Natools.S_Expressions.Lockable.Descriptor'Class);


   procedure Read_DB is new S_Expressions.Interpreter_Loop
     (Backend_Builder, Meaningless_Type, Process_User);

   procedure Read_User is new S_Expressions.Interpreter_Loop
     (User_Builder, Meaningless_Type, Process_User_Element);


   --------------------------
   -- Constructor Elements --
   --------------------------

   procedure Process_User
     (Builder : in out Backend_Builder;
      Context : in Meaningless_Type;
      Name : in S_Expressions.Atom;
      Arguments : in out Natools.S_Expressions.Lockable.Descriptor'Class)
   is
      pragma Unreferenced (Context);
      User : User_Builder (Builder.Hash_Id_First, Builder.Hash_Id_Last);
      Identity : Containers.Identity;
   begin
      Read_User (Arguments, User, Meaningless_Value);

      Identity :=
        (User => S_Expressions.Atom_Ref_Constructors.Create (Name),
         Groups => Containers.Create (User.Groups));

      for Token of User.Tokens loop
         Builder.Map.Include (Token, Identity);
      end loop;
   end Process_User;


   procedure Process_User_Element
     (Builder : in out User_Builder;
      Context : in Meaningless_Type;
      Name : in S_Expressions.Atom;
      Arguments : in out Natools.S_Expressions.Lockable.Descriptor'Class)
   is
      pragma Unreferenced (Context);
      S_Name : constant String := S_Expressions.To_String (Name);
   begin
      if S_Name = "tokens" or else S_Name = "token" then
         Containers.Append_Atoms (Builder.Tokens, Arguments);
      elsif S_Name = "groups" or else S_Name = "group" then
         Containers.Append_Atoms (Builder.Groups, Arguments);
      else
         Log (Severities.Error, "Unknown user element """ & S_Name & '"');
      end if;
   end Process_User_Element;



   ----------------------
   -- Public Interface --
   ----------------------

   overriding procedure Authenticate
     (Self : in Backend;
      Exchange : in out Exchanges.Exchange)
   is
      Cookie : constant String := Exchange.Cookie (Cookie_Name);
      Identity : Containers.Identity;
   begin
      if Cookie'Length > 3
        and then Cookie (Cookie'First) = Hash_Mark
        and then Cookie (Cookie'First + 2) = Hash_Mark
        and then not Self.Hashed.Is_Empty
        and then Cookie (Cookie'First + 1) in Self.Hashed.Query.Data.all'Range
      then
         declare
            Token : constant S_Expressions.Atom
              := S_Expressions.To_Atom
                 (Cookie (Cookie'First + 3 .. Cookie'Last));
            Hash : constant S_Expressions.Atom
              := Hash_Function_DB.Query.Data (Cookie (Cookie'First + 1)).all
                 (Token);
            Cursor : constant Token_Maps.Cursor
              := Self.Hashed.Query.Data (Cookie (Cookie'First + 1)).Find
                 (Hash);
         begin
            if Token_Maps.Has_Element (Cursor) then
               Identity := Token_Maps.Element (Cursor);
            end if;
         end;
      else
         declare
            Cursor : constant Token_Maps.Cursor
              := Self.Map.Find (S_Expressions.To_Atom (Cookie));
         begin
            if Token_Maps.Has_Element (Cursor) then
               Identity := Token_Maps.Element (Cursor);
            end if;
         end;
      end if;

      Exchange.Set_Identity (Identity);
   end Authenticate;


   function Create
     (Arguments : in out S_Expressions.Lockable.Descriptor'Class)
     return ACL.Backend'Class
   is
      Hash_Id_First : constant Hash_Id
        := (if Hash_Function_DB.Is_Empty
            then Hash_Id'Last
            else Hash_Function_DB.Query.Data.all'First);
      Hash_Id_Last : constant Hash_Id
        := (if Hash_Function_DB.Is_Empty
            then Hash_Id'First
            else Hash_Function_DB.Query.Data.all'Last);
      Builder : Backend_Builder (Hash_Id_First, Hash_Id_Last);
   begin
      case Arguments.Current_Event is
         when S_Expressions.Events.Open_List =>
            Read_DB (Arguments, Builder, Meaningless_Value);

         when S_Expressions.Events.Add_Atom =>
            declare
               Reader : S_Expressions.File_Readers.S_Reader
                 := S_Expressions.File_Readers.Reader
                    (S_Expressions.To_String (Arguments.Current_Atom));
            begin
               Read_DB (Reader, Builder, Meaningless_Value);
            end;

         when others =>
            Log (Severities.Error, "Unable to create ACL from S-expression"
              & " starting with " & Arguments.Current_Event'Img);
      end case;

      return Backend'(Map => Token_Maps.Create (Builder.Map),
                      Hashed => <>);
   end Create;


   procedure Register
     (Id : in Character;
      Fn : in Hash_Function) is
   begin
      if Fn = null then
         null;
      elsif Hash_Function_DB.Is_Empty then
         Hash_Function_DB.Replace (new Hash_Function_Array'(Id => Fn));
      elsif Id in Hash_Function_DB.Query.Data.all'Range then
         Hash_Function_DB.Update.Data.all (Id) := Fn;
      else
         declare
            New_First : constant Hash_Id
              := Hash_Id'Min (Id, Hash_Function_DB.Query.Data.all'First);
            New_Last : constant Hash_Id
              := Hash_Id'Max (Id, Hash_Function_DB.Query.Data.all'Last);
            New_Data : constant Hash_Function_Array_Refs.Data_Access
              := new Hash_Function_Array'(New_First .. New_Last => null);
         begin
            New_Data (Hash_Function_DB.Query.Data.all'First
                  ..  Hash_Function_DB.Query.Data.all'Last)
              := Hash_Function_DB.Query.Data.all;
            Hash_Function_DB.Replace (New_Data);
         end;
      end if;
   end Register;

end Natools.Web.ACL.Sx_Backends;
