------------------------------------------------------------------------------
-- Copyright (c) 2015, Natacha Porté                                        --
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
-- Natools.Web.Backends provides an interface for stream-based backends     --
-- used in web elements.                                                    --
-- Even though how parameters are used by implementations is not defined,   --
-- the following constraints apply:                                         --
--   * Directory and Name have filesystem-like semantics, in that together  --
--     they define a unique stream, but distinct streams can have the same  --
--     name in a different directory, or different names in the same        --
--     directory.                                                           --
--   * Directories can be enumerated to reach all the files it contains.    --
------------------------------------------------------------------------------

with Ada.Streams;
with Natools.S_Expressions;

package Natools.Web.Backends is
   pragma Preelaborate;

   type Backend is interface;

   function Create
     (Self : in out Backend;
      Directory, Name : in S_Expressions.Atom)
     return Ada.Streams.Root_Stream_Type'Class is abstract;
      --  Create a new file, which must not exist previously

   procedure Delete
     (Self : in out Backend;
      Directory, Name : in S_Expressions.Atom) is abstract;
      --  Destroy a file, which must exist previously

   function Read
     (Self : in Backend;
      Directory, Name : in S_Expressions.Atom)
     return Ada.Streams.Root_Stream_Type'Class is abstract;
      --  Read the contents of an existing file

   function Append
     (Self : in out Backend;
      Directory, Name : in S_Expressions.Atom)
     return Ada.Streams.Root_Stream_Type'Class is abstract;
      --  Return a stream to append data to the given file

   function Overwrite
     (Self : in out Backend;
      Directory, Name : in S_Expressions.Atom)
     return Ada.Streams.Root_Stream_Type'Class is abstract;
      --  Reset the given file to empty and return a stream to write on it

   procedure Iterate
     (Self : in Backend;
      Directory : in S_Expressions.Atom;
      Process : not null access procedure (Name : in S_Expressions.Atom))
     is abstract;
      --  Iterate over all the existing file names in Directory

end Natools.Web.Backends;
