------------------------------------------------------------------------------
-- Copyright (c) 2019, Natacha Porté                                        --
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

with Ada.IO_Exceptions;
with Ada.Strings.Fixed;
with Natools.S_Expressions.Atom_Ref_Constructors;
with Natools.S_Expressions.Enumeration_IO;
with Natools.S_Expressions.File_Readers;
with Natools.S_Expressions.Interpreter_Loop;
with Natools.S_Expressions.Lockable;
with Natools.Web.Error_Pages;

package body Natools.Web.Cookie_Setters is

   package Elements is
      type Enum is
        (Unknown,
         Redirect_Target,
         Force_Name,
         Allowed_Names,
         Path,
         Comment,
         Domain,
         Max_Age,
         Secure,
         No_Secure,
         HTTP_Only,
         No_HTTP_Only,
         Max_Length);
   end Elements;

   package Element_IO is new S_Expressions.Enumeration_IO.Typed_IO
     (Elements.Enum);


   procedure Execute
     (Object : in out Setter;
      Context : in Meaningless_Type;
      Name : in S_Expressions.Atom;
      Arguments : in out S_Expressions.Lockable.Descriptor'Class);

   procedure Set_Cookie
     (Object : in Setter;
      Exchange : in out Sites.Exchange;
      Key : in String;
      Value : in String)
     with Pre => Key'Length >= 1;

   function To_Element (Name : in S_Expressions.Atom) return Elements.Enum;

   procedure Update_Setter is new S_Expressions.Interpreter_Loop
     (Setter, Meaningless_Type, Execute);


   ------------------------------
   -- Local Helper Subprograms --
   ------------------------------

   procedure Execute
     (Object : in out Setter;
      Context : in Meaningless_Type;
      Name : in S_Expressions.Atom;
      Arguments : in out S_Expressions.Lockable.Descriptor'Class)
   is
      pragma Unreferenced (Context);
      use Elements;
      use type S_Expressions.Events.Event;
   begin
      case To_Element (Name) is
         when Unknown =>
            Log (Severities.Error,
              "Unknown cookie setter element """
              & S_Expressions.To_String (Name) & '"');

         when Redirect_Target =>
            if Arguments.Current_Event = S_Expressions.Events.Add_Atom then
               Object.Redirect_Target
                 := S_Expressions.Atom_Ref_Constructors.Create
                    (Arguments.Current_Atom);
            end if;

         when Force_Name =>
            if Arguments.Current_Event = S_Expressions.Events.Add_Atom then
               Object.Force_Name
                 := S_Expressions.Atom_Ref_Constructors.Create
                    (Arguments.Current_Atom);
            else
               Object.Force_Name.Reset;
            end if;

         when Allowed_Names =>
            declare
               Names : Containers.Unsafe_Atom_Lists.List;
            begin
               Containers.Append_Atoms (Names, Arguments);
               Object.Allowed_Names := Containers.Create (Names);
            end;

         when Path =>
            if Arguments.Current_Event = S_Expressions.Events.Add_Atom then
               Object.Path
                 := S_Expressions.Atom_Ref_Constructors.Create
                    (Arguments.Current_Atom);
            else
               Object.Path.Reset;
            end if;

         when Comment =>
            if Arguments.Current_Event = S_Expressions.Events.Add_Atom then
               Object.Comment
                 := S_Expressions.Atom_Ref_Constructors.Create
                    (Arguments.Current_Atom);
            else
               Object.Comment.Reset;
            end if;

         when Domain =>
            if Arguments.Current_Event = S_Expressions.Events.Add_Atom then
               Object.Domain
                 := S_Expressions.Atom_Ref_Constructors.Create
                    (Arguments.Current_Atom);
            else
               Object.Domain.Reset;
            end if;

         when Max_Age =>
            if Arguments.Current_Event = S_Expressions.Events.Add_Atom then
               begin
                  Object.Max_Age := Duration'Value
                    (S_Expressions.To_String (Arguments.Current_Atom));
               exception
                  when Constraint_Error =>
                     Object.Max_Age := Default_Max_Age;
               end;
            end if;

         when Secure =>
            Object.Secure := True;

         when No_Secure =>
            Object.Secure := False;

         when HTTP_Only =>
            Object.HTTP_Only := True;

         when No_HTTP_Only =>
            Object.HTTP_Only := False;

         when Max_Length =>
            if Arguments.Current_Event = S_Expressions.Events.Add_Atom then
               begin
                  Object.Max_Length := Natural'Value
                    (S_Expressions.To_String (Arguments.Current_Atom));
               exception
                  when Constraint_Error =>
                     Object.Max_Length := Default_Max_Length;
               end;
            end if;
      end case;
   end Execute;


   procedure Set_Cookie
     (Object : in Setter;
      Exchange : in out Sites.Exchange;
      Key : in String;
      Value : in String)
   is
      use type Containers.Atom_Set;
   begin
      if Object.Force_Name.Is_Empty
        and then Object.Allowed_Names /= Containers.Null_Atom_Set
        and then not Containers.Contains
           (Object.Allowed_Names, S_Expressions.To_Atom (Key))
      then
         return;
      end if;

      Exchange.Set_Cookie
        (Key => Key,
         Value => Value,
         Comment => Comment (Object),
         Domain => Domain (Object),
         Max_Age => (if Value'Length > 0 then Object.Max_Age else 0.0),
         Path => Path (Object),
         Secure => Object.Secure,
         HTTP_Only => Object.HTTP_Only);
   end Set_Cookie;


   function To_Element (Name : in S_Expressions.Atom) return Elements.Enum is
      Result : Elements.Enum := Elements.Unknown;
   begin
      begin
         Result := Element_IO.Value (Name);
      exception
         when Constraint_Error => null;
      end;

      return Result;
   end To_Element;



   ----------------------
   -- Public Interface --
   ----------------------

   function Create (File : in S_Expressions.Atom)
     return Sites.Page_Loader'Class is
   begin
      return Loader'(File_Name
        => S_Expressions.Atom_Ref_Constructors.Create (File));
   end Create;


   overriding procedure Load
     (Object : in out Loader;
      Builder : in out Sites.Site_Builder;
      Path : in S_Expressions.Atom)
   is
      pragma Unmodified (Object);
      Page : Setter;
   begin
      declare
         File_Name : constant String
           := S_Expressions.To_String (Object.File_Name.Query);
      begin
         declare
            Reader : S_Expressions.File_Readers.S_Reader
              := S_Expressions.File_Readers.Reader (File_Name);
         begin
            Update_Setter (Reader, Page, Meaningless_Value);
         end;

         if Page.Redirect_Target.Is_Empty then
            Log (Severities.Error,
                 "Cookie setter file """ & File_Name
                 & """ defines no rediction target");
            return;
         end if;
      exception
         when Ada.IO_Exceptions.Name_Error =>
            Log (Severities.Warning,
                 "Unable to open cookie setter file """ & File_Name
                 & """, using it as a rediction target");
            Page.Redirect_Target := Object.File_Name;
      end;

      Sites.Insert (Builder, Path, Page);
   end Load;


   overriding procedure Respond
     (Object : in out Setter;
      Exchange : in out Sites.Exchange;
      Extra_Path : in S_Expressions.Atom)
   is
      pragma Unmodified (Object);
      use type S_Expressions.Octet;
      use type S_Expressions.Offset;

      Arguments : constant String
        := (if Extra_Path'Length >= 1
               and then Extra_Path (Extra_Path'First) = Character'Pos ('/')
            then S_Expressions.To_String
               (Extra_Path (Extra_Path'First + 1 .. Extra_Path'Last))
            else S_Expressions.To_String (Extra_Path));
      Separator : constant Natural := Ada.Strings.Fixed.Index (Arguments, "/");
   begin
      if Arguments'Length > Object.Max_Length then
         return;
      elsif not Object.Force_Name.Is_Empty then
         Set_Cookie
           (Object,
            Exchange,
            S_Expressions.To_String (Object.Force_Name.Query),
            Arguments);
      elsif Arguments'Length = 0 or Separator = Arguments'First then
         return;
      elsif Separator = 0 or Separator = Arguments'Last then
         Set_Cookie
           (Object,
            Exchange,
            Arguments,
            "");
      else
         Set_Cookie
           (Object,
            Exchange,
            Arguments (Arguments'First .. Separator - 1),
            Arguments (Separator + 1 .. Arguments'Last));
      end if;

      Natools.Web.Error_Pages.See_Other
        (Exchange,
         Object.Redirect_Target.Query);
   end Respond;

end Natools.Web.Cookie_Setters;
