------------------------------------------------------------------------------
-- Copyright (c) 2014, Natacha Porté                                        --
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

with AWS.Response.Set;
with AWS.MIME;
with Natools.S_Expressions.Atom_Ref_Constructors;

package body Natools.Web.Exchanges is

   package Constructors renames Natools.S_Expressions.Atom_Ref_Constructors;

   procedure Ensure_Kind
     (Object : in out Exchange;
      Kind : in Responses.Kind);
      --  Switch Object.Kind to Kind, resetting internal state if needed


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

   function Method (Object : Exchange) return Request_Method is
   begin
      case AWS.Status.Method (Object.Request.all) is
         when AWS.Status.GET  => return GET;
         when AWS.Status.HEAD => return HEAD;
         when AWS.Status.POST => return POST;
         when others          => return Unknown_Method;
      end case;
   end Method;


   function Path (Object : Exchange) return String is
   begin
      return AWS.Status.URI (Object.Request.all);
   end Path;



   ---------------------------
   -- Response Construction --
   ---------------------------

   procedure Append
     (Object : in out Exchange;
      Data : in S_Expressions.Atom) is
   begin
      Ensure_Kind (Object, Responses.Buffer);
      Object.Response_Body.Append (Data);
   end Append;


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


   procedure Send_File
     (Object : in out Exchange;
      File_Name : in S_Expressions.Atom) is
   begin
      Ensure_Kind (Object, Responses.File);
      Object.Response_Body.Soft_Reset;
      Object.Response_Body.Append (File_Name);
   end Send_File;


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

      return Result;
   end Response;

end Natools.Web.Exchanges;
