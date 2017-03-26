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

------------------------------------------------------------------------------
-- Natools.Web.Exchanges provides a type representing a single immutable    --
-- HTTP request and the associated response being bulit.                    --
-- It is mostly meant as an abstraction of the HTTP backend. The            --
-- implementation here sits on top of AWS, though a similar interface       --
-- should be easy to built for other HTTP backends.                         --
------------------------------------------------------------------------------

with Ada.Streams;
with AWS.Response;
with AWS.Status;
with Natools.S_Expressions.Atom_Refs;
with Natools.Web.Containers;
with Natools.Web.Filters;

private with AWS.Messages;
private with Natools.S_Expressions.Atom_Buffers;

package Natools.Web.Exchanges is

   type Request_Method is (GET, HEAD, POST, Unknown_Method);
   subtype Known_Method is Request_Method range GET .. POST;
   type Method_Array is array (Positive range <>) of Known_Method;
   type Method_Set is private;

   function To_Set (List : Method_Array) return Method_Set;
   function Is_In (Method : Request_Method; Set : Method_Set) return Boolean;

   function Image (List : Method_Array) return String;
   function Image (Set : Method_Set) return String;



   type Exchange (Request : access constant AWS.Status.Data)
     is limited new Ada.Streams.Root_Stream_Type with private;

   overriding procedure Read
     (Stream : in out Exchange;
      Item : out Ada.Streams.Stream_Element_Array;
      Last : out Ada.Streams.Stream_Element_Offset);

   function Cookie (Object : in Exchange; Name : in String) return String;
      --  Return a specific cookie, or the empty string if it is not set

   function Cookie_Table
     (Object : in Exchange)
     return Containers.Atom_Table_Refs.Immutable_Reference;
      --  Return all the cookies as a two-column table

   procedure Iterate_Parameters
     (Object : in Exchange;
      Process : not null access procedure (Name, Value : String));
      --  Iterate over request parameters

   function Method (Object : Exchange) return Request_Method;
      --  Method requested by client

   function Parameter
     (Object : Exchange;
      Name : String)
     return String;
      --  Access a specific parameter

   function Path (Object : Exchange) return String;
      --  Path part of the requested URL


   function Has_Identity (Object : Exchange) return Boolean;
      --  Return whether an identity has been stored in Object

   function Identity (Object : Exchange) return Containers.Identity
     with Pre => Has_Identity (Object);
      --  Return the identity stored in Objet

   procedure Set_Identity
     (Object : in out Exchange;
      Identity : in Containers.Identity);
      --  Store an identity inside Object


   function Has_Response (Object : Exchange) return Boolean;
      --  Return whether a response has been built in Object

   function Response (Object : Exchange) return AWS.Response.Data;
      --  Return AWS response representation built in Object


   procedure Append
     (Object : in out Exchange;
      Data : in S_Expressions.Atom);
      --  Append Data to internal memory buffer in Object

   overriding procedure Write
     (Stream : in out Exchange;
      Data : in Ada.Streams.Stream_Element_Array)
     renames Append;

   procedure Insert_Filter
     (Object : in out Exchange;
      Filter : in Filters.Filter'Class;
      Side : in Filters.Side := Filters.Top);

   procedure Remove_Filter
     (Object : in out Exchange;
      Filter : in Filters.Filter'Class;
      Side : in Filters.Side := Filters.Top);

   procedure Send_File
     (Object : in out Exchange;
      File_Name : in S_Expressions.Atom);
      --  Send File_Name as a response in Object

   procedure Set_MIME_Type
     (Object : in out Exchange;
      MIME_Type : in S_Expressions.Atom);
      --  Set the MIME type for the response


   procedure Method_Not_Allowed
     (Object : in out Exchange;
      Allow : in Method_Set);
      --  Set internal state to HTTP 405 Method Not Allowed

   procedure Not_Found (Object : in out Exchange);
      --  Set internal state to HTTP 404 Not Found

   procedure Permanent_Redirect
     (Object : in out Exchange;
      Target : in S_Expressions.Atom);
   procedure Permanent_Redirect
     (Object : in out Exchange;
      Target : in S_Expressions.Atom_Refs.Immutable_Reference);
      --  Set internal state to HTTP 301 Moved Permanently

   procedure See_Other
     (Object : in out Exchange;
      Target : in S_Expressions.Atom);
   procedure See_Other
     (Object : in out Exchange;
      Target : in S_Expressions.Atom_Refs.Immutable_Reference);
      --  Set internal state to HTTP 303 See Other

private

   type Method_Set is array (Known_Method) of Boolean with Pack;

   function Is_In (Method : Request_Method; Set : Method_Set) return Boolean
     is (Method in Set'Range and then Set (Method));

   package Responses is
      type Kind is (Empty, Buffer, File);
   end Responses;

   type Exchange (Request : access constant AWS.Status.Data)
     is limited new Ada.Streams.Root_Stream_Type with record
      Allow : Method_Set := (others => False);
      Filter : Filters.Stack;
      Kind : Responses.Kind := Responses.Empty;
      Location : S_Expressions.Atom_Refs.Immutable_Reference;
      MIME_Type : S_Expressions.Atom_Refs.Immutable_Reference;
      Response_Body : S_Expressions.Atom_Buffers.Atom_Buffer;
      Status_Code : AWS.Messages.Status_Code := AWS.Messages.S200;
      Has_Identity : Boolean := False;
      Identity : Containers.Identity;
   end record;

   function Has_Response (Object : Exchange) return Boolean
     is (not Responses."=" (Object.Kind, Responses.Empty));

   function Has_Identity (Object : Exchange) return Boolean
     is (Object.Has_Identity);

end Natools.Web.Exchanges;
