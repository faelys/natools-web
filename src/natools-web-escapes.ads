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
-- Natools.Web.Escapes provdes primitives for HTML-style escaping.          --
------------------------------------------------------------------------------

with Ada.Streams;
with Natools.S_Expressions.Atom_Refs;

package Natools.Web.Escapes is
   pragma Preelaborate;

   type Escape_Set is record
      Gt_Lt : Boolean := False;
      Amp : Boolean := False;
      Apos : Boolean := False;
      Quot : Boolean := False;
   end record;

   function Is_Empty (Set : in Escape_Set) return Boolean
     is (Set.Gt_Lt or Set.Amp or Set.Apos or Set.Quot);

   HTML_Attribute : constant Escape_Set;
   HTML_Body : constant Escape_Set;


   function Escaped_Length
     (Data : in S_Expressions.Atom;
      Set : in Escape_Set)
     return S_Expressions.Count;
      --  Return the number of octet in the escaped version of Data

   procedure Write
     (Output : in out Ada.Streams.Root_Stream_Type'Class;
      Data : in S_Expressions.Atom;
      Set : in Escape_Set);
      --  Escape octets from Data in Set, and write them into Output

   procedure Write
     (Output : in out Ada.Streams.Root_Stream_Type'Class;
      Text : in String;
      Set : in Escape_Set);
      --  Escape octets from Text in Set, and write them into Output


   function Escape
     (Data : in S_Expressions.Atom;
      Set : in Escape_Set)
     return S_Expressions.Atom;
      --  Escape Data and return it directly as an atom

   function Escape
     (Data : in S_Expressions.Atom;
      Set : in Escape_Set)
     return S_Expressions.Atom_Refs.Immutable_Reference;
      --  Escape Data and return it in a newly-created reference

   function Escape
     (Data : in S_Expressions.Atom_Refs.Immutable_Reference;
      Set : in Escape_Set)
     return S_Expressions.Atom_Refs.Immutable_Reference;
      --  Escape Data if needed, otherwise duplicate the reference

private

   type Atom_Stream (Data : not null access S_Expressions.Atom)
     is new Ada.Streams.Root_Stream_Type
   with record
      Last : S_Expressions.Offset;
   end record;

   overriding procedure Read
     (Stream : in out Atom_Stream;
      Item : out Ada.Streams.Stream_Element_Array;
      Last : out Ada.Streams.Stream_Element_Offset);

   overriding procedure Write
     (Stream : in out Atom_Stream;
      Item : in Ada.Streams.Stream_Element_Array);


   type Count_Stream is new Ada.Streams.Root_Stream_Type with record
      Count : S_Expressions.Count := 0;
   end record;

   overriding procedure Read
     (Stream : in out Count_Stream;
      Item : out Ada.Streams.Stream_Element_Array;
      Last : out Ada.Streams.Stream_Element_Offset);

   overriding procedure Write
     (Stream : in out Count_Stream;
      Item : in Ada.Streams.Stream_Element_Array);


   type Octet_Set is array (S_Expressions.Octet) of Boolean;

   procedure Update_Set (Octets : in out Octet_Set; Set : in Escape_Set);
      --  Convert an escape set to an octet set


   HTML_Attribute : constant Escape_Set := (Apos => False, others => True);

   HTML_Body : constant Escape_Set
     := (Gt_Lt | Amp => True, others => False);

end Natools.Web.Escapes;
