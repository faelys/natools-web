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

------------------------------------------------------------------------------
-- Natools.Web.Exchanges provides a type representing a single immutable    --
-- HTTP request and the associated response being bulit.                    --
-- It is mostly meant as an abstraction of the HTTP backend. The            --
-- implementation here sits on top of AWS, though a similar interface       --
-- should be easy to built for other HTTP backends.                         --
------------------------------------------------------------------------------

with AWS.Response;
with AWS.Status;
with Natools.S_Expressions;

private with Natools.S_Expressions.Atom_Buffers;
private with Natools.S_Expressions.Atom_Refs;

package Natools.Web.Exchanges is

   type Exchange (Request : access constant AWS.Status.Data)
     is limited private;

   function Path (Object : Exchange) return String;
      --  Path part of the requested URL


   function Has_Response (Object : Exchange) return Boolean;
      --  Return whether a response has been built in Object

   function Response (Object : Exchange) return AWS.Response.Data;
      --  Return AWS response representation built in Object


   procedure Append
     (Object : in out Exchange;
      Data : in S_Expressions.Atom);
      --  Append Data to internal memory buffer in Object

   procedure Send_File
     (Object : in out Exchange;
      File_Name : in S_Expressions.Atom);
      --  Send File_Name as a response in Object

private

   package Responses is
      type Kind is (Empty, Buffer, File);
   end Responses;

   type Exchange (Request : access constant AWS.Status.Data) is record
      Kind : Responses.Kind := Responses.Empty;
      MIME_Type : S_Expressions.Atom_Refs.Immutable_Reference;
      Response_Body : S_Expressions.Atom_Buffers.Atom_Buffer;
   end record;

   function Has_Response (Object : Exchange) return Boolean
     is (not Responses."=" (Object.Kind, Responses.Empty));

end Natools.Web.Exchanges;
