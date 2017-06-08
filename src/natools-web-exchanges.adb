------------------------------------------------------------------------------
-- Copyright (c) 2014-2017, Natacha Porté                                   --
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

with Ada.Strings.Unbounded;
with AWS.Cookie;
with AWS.Headers.Values;
with AWS.MIME;
with AWS.Parameters;
with AWS.Response.Set;
with Natools.S_Expressions.Atom_Ref_Constructors;

package body Natools.Web.Exchanges is

   package Constructors renames Natools.S_Expressions.Atom_Ref_Constructors;

   procedure Ensure_Kind
     (Object : in out Exchange;
      Kind : in Responses.Kind);
      --  Switch Object.Kind to Kind, resetting internal state if needed

   function Make_Row (Key, Value : in String)
     return Containers.Atom_Array_Refs.Immutable_Reference;
      --  Create an atom table row from key and value


   ------------------------------
   -- Local Helper Subprograms --
   ------------------------------

   procedure Ensure_Kind
     (Object : in out Exchange;
      Kind : in Responses.Kind)
   is
      use type Responses.Kind;
   begin
      if Object.Kind /= Kind then
         Object.Kind := Kind;
         Object.Response_Body.Soft_Reset;
      end if;
   end Ensure_Kind;


   function Make_Row (Key, Value : in String)
     return Containers.Atom_Array_Refs.Immutable_Reference
   is
      Data : constant Containers.Atom_Array_Refs.Data_Access
        := new Containers.Atom_Array (1 .. 2);
      Result : constant Containers.Atom_Array_Refs.Immutable_Reference
        := Containers.Atom_Array_Refs.Create (Data);
   begin
      Data (1) := Constructors.Create (S_Expressions.To_Atom (Key));
      Data (2) := Constructors.Create (S_Expressions.To_Atom (Value));
      return Result;
   end Make_Row;



   --------------------------------
   -- Request Method Subprograms --
   --------------------------------

   function To_Set (List : Method_Array) return Method_Set is
      Result : Method_Set := (others => False);
   begin
      for I in List'Range loop
         Result (List (I)) := True;
      end loop;

      return Result;
   end To_Set;


   function Image (List : Method_Array) return String is
   begin
      return Image (To_Set (List));
   end Image;


   function Image (Set : Method_Set) return String is
      Buffer : String := "GET, HEAD, POST";
      First : Boolean := True;
      Last : Natural := Buffer'First - 1;

      procedure Raw_Append (S : in String);
      procedure Append (S : in String);

      procedure Raw_Append (S : in String) is
      begin
         Buffer (Last + 1 .. Last + S'Length) := S;
         Last := Last + S'Length;
      end Raw_Append;

      procedure Append (S : in String) is
      begin
         if First then
            First := False;
         else
            Raw_Append (", ");
         end if;

         Raw_Append (S);
      end Append;
   begin
      for M in Set'Range loop
         if Set (M) then
            Append (Request_Method'Image (M));
         end if;
      end loop;

      return Buffer (Buffer'First .. Last);
   end Image;



   -----------------------
   -- Request Accessors --
   -----------------------

   function Cookie (Object : in Exchange; Name : in String) return String is
   begin
      return AWS.Cookie.Get (Object.Request.all, Name);
   end Cookie;


   function Cookie_Table
     (Object : in Exchange)
     return Containers.Atom_Table_Refs.Immutable_Reference
   is
      Headers : constant AWS.Headers.List
        := AWS.Status.Header (Object.Request.all);
      Cookies : constant String
        := AWS.Headers.Get_Values (Headers, AWS.Messages.Cookie_Token);
      Headers_Set : constant AWS.Headers.Values.Set
        := AWS.Headers.Values.Split (Cookies);
      List : Containers.Atom_Row_Lists.List;
   begin
      for I in Headers_Set'Range loop
         if Headers_Set (I).Named_Value then
            List.Append (Make_Row
              (Ada.Strings.Unbounded.To_String (Headers_Set (I).Name),
               Ada.Strings.Unbounded.To_String (Headers_Set (I).Value)));
         else
            Log (Severities.Error, "Nameless cookie """
              & Ada.Strings.Unbounded.To_String (Headers_Set (I).Value) & '"');
         end if;
      end loop;

      return Containers.Create (List);
   end Cookie_Table;


   function Has_Parameter (Object : Exchange; Name : String) return Boolean is
   begin
      return AWS.Parameters.Exist
        (AWS.Status.Parameters (Object.Request.all),
         Name);
   end Has_Parameter;


   function Header (Object : Exchange; Name : String) return String is
   begin
      return AWS.Headers.Get_Values
        (AWS.Status.Header (Object.Request.all),
         Name);
   end Header;


   procedure Iterate_Parameters
     (Object : in Exchange;
      Process : not null access procedure (Name, Value : String))
   is
      Parameters : constant AWS.Parameters.List
        := AWS.Status.Parameters (Object.Request.all);
   begin
      for I in 1 .. AWS.Parameters.Count (Parameters) loop
         Process.all
           (AWS.Parameters.Get_Name (Parameters, I),
            AWS.Parameters.Get_Value (Parameters, I));
      end loop;
   end Iterate_Parameters;


   function Method (Object : Exchange) return Request_Method is
   begin
      case AWS.Status.Method (Object.Request.all) is
         when AWS.Status.GET  => return GET;
         when AWS.Status.HEAD => return HEAD;
         when AWS.Status.POST => return POST;
         when others          => return Unknown_Method;
      end case;
   end Method;


   function Parameter
     (Object : Exchange;
      Name : String)
     return String is
   begin
      return AWS.Parameters.Get
        (AWS.Status.Parameters (Object.Request.all), Name);
   end Parameter;


   overriding procedure Read
     (Stream : in out Exchange;
      Item : out Ada.Streams.Stream_Element_Array;
      Last : out Ada.Streams.Stream_Element_Offset)
   is
      pragma Unreferenced (Stream, Item, Last);
   begin
      raise Program_Error with "Reading exchange stream is not implemented";
   end Read;


   function Path (Object : Exchange) return String is
   begin
      return AWS.Status.URI (Object.Request.all);
   end Path;



   -------------------------
   -- Identity Management --
   -------------------------

   function Identity (Object : Exchange) return Containers.Identity is
   begin
      return Object.Identity;
   end Identity;


   procedure Set_Identity
     (Object : in out Exchange;
      Identity : in Containers.Identity) is
   begin
      Object.Has_Identity := True;
      Object.Identity := Identity;
   end Set_Identity;



   ---------------------------
   -- Response Construction --
   ---------------------------

   procedure Append
     (Object : in out Exchange;
      Data : in S_Expressions.Atom) is
   begin
      Ensure_Kind (Object, Responses.Buffer);
      Object.Filter.Apply (Object.Response_Body, Data);
   end Append;


   procedure Insert_Filter
     (Object : in out Exchange;
      Filter : in Filters.Filter'Class;
      Side : in Filters.Side := Filters.Top) is
   begin
      Object.Filter.Insert (Filter, Side);
   end Insert_Filter;


   procedure Method_Not_Allowed
     (Object : in out Exchange;
      Allow : in Method_Set) is
   begin
      Object.Status_Code := AWS.Messages.S405;
      Object.Allow := Allow;
   end Method_Not_Allowed;


   procedure Not_Found (Object : in out Exchange) is
   begin
      Object.Status_Code := AWS.Messages.S404;
   end Not_Found;


   procedure Remove_Filter
     (Object : in out Exchange;
      Filter : in Filters.Filter'Class;
      Side : in Filters.Side := Filters.Top) is
   begin
      Object.Filter.Remove (Filter, Side);
   end Remove_Filter;


   procedure Send_File
     (Object : in out Exchange;
      File_Name : in S_Expressions.Atom) is
   begin
      Ensure_Kind (Object, Responses.File);
      Object.Response_Body.Soft_Reset;
      Object.Response_Body.Append (File_Name);
   end Send_File;


   procedure Set_Cookie
     (Object : in out Exchange;
      Key : in String;
      Value : in String) is
   begin
      Object.Set_Cookies.Include (Key, Value);
   end Set_Cookie;


   procedure Set_MIME_Type
     (Object : in out Exchange;
      MIME_Type : in S_Expressions.Atom)
   is
      use type S_Expressions.Count;
   begin
      if Object.Response_Body.Length /= 0 then
         Log (Severities.Warning,
           "Changing MIME type of partially-created response");
      end if;

      Object.MIME_Type
        := S_Expressions.Atom_Ref_Constructors.Create (MIME_Type);
   end Set_MIME_Type;


   procedure Permanent_Redirect
     (Object : in out Exchange;
      Target : in S_Expressions.Atom) is
   begin
      Object.Status_Code := AWS.Messages.S301;
      Object.Location := Constructors.Create (Target);
   end Permanent_Redirect;


   procedure Permanent_Redirect
     (Object : in out Exchange;
      Target : in S_Expressions.Atom_Refs.Immutable_Reference) is
   begin
      Object.Status_Code := AWS.Messages.S301;
      Object.Location := Target;
   end Permanent_Redirect;


   procedure See_Other
     (Object : in out Exchange;
      Target : in S_Expressions.Atom) is
   begin
      See_Other (Object, Constructors.Create (Target));
   end See_Other;


   procedure See_Other
     (Object : in out Exchange;
      Target : in S_Expressions.Atom_Refs.Immutable_Reference) is
   begin
      if AWS.Status.HTTP_Version (Object.Request.all) = AWS.HTTP_10 then
         Object.Status_Code := AWS.Messages.S302;
      else
         Object.Status_Code := AWS.Messages.S303;
      end if;

      Object.Location := Target;
   end See_Other;


   ---------------------
   -- Response Export --
   ---------------------

   function Response (Object : Exchange) return AWS.Response.Data is
      Result : AWS.Response.Data;
   begin
      case Object.Kind is
         when Responses.Empty =>
            raise Program_Error with "Empty response cannot be exported";

         when Responses.Buffer =>
            if Object.MIME_Type.Is_Empty then
               Result := AWS.Response.Build
                 ("text/html",
                  Object.Response_Body.Data,
                  Object.Status_Code);
            else
               Result := AWS.Response.Build
                 (S_Expressions.To_String (Object.MIME_Type.Query.Data.all),
                  Object.Response_Body.Data,
                  Object.Status_Code);
            end if;

         when Responses.File =>
            if Object.MIME_Type.Is_Empty then
               declare
                  Filename : constant String
                    := S_Expressions.To_String (Object.Response_Body.Data);
               begin
                  Result := AWS.Response.File
                    (AWS.MIME.Content_Type (Filename),
                     Filename,
                     Object.Status_Code);
               end;
            else
               Result := AWS.Response.File
                 (S_Expressions.To_String (Object.MIME_Type.Query.Data.all),
                  S_Expressions.To_String (Object.Response_Body.Data),
                  Object.Status_Code);
            end if;
      end case;

      if Object.Allow /= Method_Set'(others => False) then
         AWS.Response.Set.Add_Header
           (Result,
            AWS.Messages.Allow_Token,
            Image (Object.Allow));
      end if;

      if not Object.Location.Is_Empty then
         AWS.Response.Set.Add_Header
           (Result,
            AWS.Messages.Location_Token,
            S_Expressions.To_String (Object.Location.Query));
      end if;

      for Cursor in Object.Set_Cookies.Iterate loop
         AWS.Cookie.Set
           (Result,
            String_Maps.Key (Cursor),
            String_Maps.Element (Cursor));
      end loop;

      return Result;
   end Response;

end Natools.Web.Exchanges;
