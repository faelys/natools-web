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

with AWS.MIME;

package body Natools.Web.Exchanges is

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



   -----------------------
   -- Request Accessors --
   -----------------------

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


   ---------------------
   -- Response Export --
   ---------------------

   function Response (Object : Exchange) return AWS.Response.Data is
   begin
      case Object.Kind is
         when Responses.Empty =>
            raise Program_Error with "Empty response cannot be exported";

         when Responses.Buffer =>
            if Object.MIME_Type.Is_Empty then
               return AWS.Response.Build
                 ("text/html",
                  Object.Response_Body.Data,
                  Object.Status_Code);
            else
               return AWS.Response.Build
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
                  return AWS.Response.File
                    (AWS.MIME.Content_Type (Filename),
                     Filename,
                     Object.Status_Code);
               end;
            else
               return AWS.Response.File
                 (S_Expressions.To_String (Object.MIME_Type.Query.Data.all),
                  S_Expressions.To_String (Object.Response_Body.Data),
                  Object.Status_Code);
            end if;
      end case;
   end Response;

end Natools.Web.Exchanges;
