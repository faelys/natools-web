------------------------------------------------------------------------------
-- Copyright (c) 2017, Natacha Porté                                        --
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
-- Natools.Web.Filters.Text_Replacement provides a filter which directly    --
-- replaces a piece of text with another.                                   --
-- Note that due to API limitation, patterns which cross block limits are   --
-- missed.                                                                  --
------------------------------------------------------------------------------

with Natools.S_Expressions.Lockable;

package Natools.Web.Filters.Text_Replacement is
   pragma Preelaborate;

   type Filter (<>) is new Filters.Filter with private;

   overriding procedure Apply
     (Object : in Filter;
      Output : in out Ada.Streams.Root_Stream_Type'Class;
      Data : in Ada.Streams.Stream_Element_Array);
      --  Perform text remplacement on Data

   function Create
     (Arguments : in out S_Expressions.Lockable.Descriptor'Class)
     return Filters.Filter'Class;
      --  Build a text replacement filter from two atoms, first the pattern
      --  then the replacement.

private

   type Filter
     (Pattern_Length, Replacement_Length : Natools.S_Expressions.Count)
   is new Filters.Filter with record
      Pattern : Natools.S_Expressions.Atom (1 .. Pattern_Length);
      Replacement : Natools.S_Expressions.Atom (1 .. Replacement_Length);
   end record;

end Natools.Web.Filters.Text_Replacement;
