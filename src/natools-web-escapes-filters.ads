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
-- Natools.Web.Escapes.Filters provides a Filter object around escape       --
-- primitives.                                                              --
------------------------------------------------------------------------------

with Natools.S_Expressions.Lockable;
with Natools.Web.Filters;

package Natools.Web.Escapes.Filters is
   pragma Preelaborate;

   type Filter is new Web.Filters.Filter with private;

   overriding procedure Apply
     (Object : in Filter;
      Output : in out Ada.Streams.Root_Stream_Type'Class;
      Data : in Ada.Streams.Stream_Element_Array);
      --  Apply the escape filter to Data

   function Create
     (Arguments : in out S_Expressions.Lockable.Descriptor'Class)
     return Web.Filters.Filter'Class;
      --  Create a new escape filter

private

   type Filter is new Web.Filters.Filter with record
      Set : Escape_Set;
   end record;

end Natools.Web.Escapes.Filters;
