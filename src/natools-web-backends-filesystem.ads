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
-- Natools.Web.Backends.Filesystem provides a very simple filesystem-based  --
-- implementation of Natools.Web.Backends.Backend.                          --
------------------------------------------------------------------------------

with Natools.S_Expressions.Lockable;

private with Natools.S_Expressions.Atom_Refs;

package Natools.Web.Backends.Filesystem is

   type File_Backend is new Backend with private;

   overriding function Create
     (Self : in out File_Backend;
      Directory, Name : in S_Expressions.Atom)
     return Ada.Streams.Root_Stream_Type'Class;
      --  Create a new file, which must not exist previously

   overriding procedure Delete
     (Self : in out File_Backend;
      Directory, Name : in S_Expressions.Atom);
      --  Destroy a file, which must exist previously

   overriding function Read
     (Self : in File_Backend;
      Directory, Name : in S_Expressions.Atom)
     return Ada.Streams.Root_Stream_Type'Class;
      --  Read the contents of an existing file

   overriding function Append
     (Self : in out File_Backend;
      Directory, Name : in S_Expressions.Atom)
     return Ada.Streams.Root_Stream_Type'Class;
      --  Return a stream to append data to the given file

   overriding function Overwrite
     (Self : in out File_Backend;
      Directory, Name : in S_Expressions.Atom)
     return Ada.Streams.Root_Stream_Type'Class;
      --  Reset the given file to empty and return a stream to write on it

   overriding procedure Iterate
     (Self : in File_Backend;
      Directory : in S_Expressions.Atom;
      Process : not null access procedure (Name : in S_Expressions.Atom));
      --  Iterate over all the existing file names in Directory

   not overriding function Create (Root : in String) return File_Backend;

   function Create
     (Arguments : in out S_Expressions.Lockable.Descriptor'Class)
     return Backend'Class;

private

   type File_Backend is new Backend with record
      Root : S_Expressions.Atom_Refs.Immutable_Reference;
   end record;

end Natools.Web.Backends.Filesystem;
