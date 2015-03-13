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
-- Natools.Web.Filters provides an interface for stream filters which       --
-- interface between Write primitives, and a containers for a filter stack  --
-- and a filter map.                                                        --
------------------------------------------------------------------------------

with Ada.Streams;

private with Ada.Containers.Indefinite_Doubly_Linked_Lists;

package Natools.Web.Filters is
   pragma Preelaborate;

   type Filter is interface;

   procedure Apply
     (Object : in Filter;
      Output : in out Ada.Streams.Root_Stream_Type'Class;
      Data : in Ada.Streams.Stream_Element_Array)
    is abstract;
      --  Apply the filter described by Object on Data and append it to Output


   type Stack is new Filter with private;
      --  Stack of filters

   type Side is (Top, Bottom);
      --  Side on the filter stack: Top is closest to original data,
      --  Bottom is closest to output stream.

   overriding procedure Apply
     (Object : in Stack;
      Output : in out Ada.Streams.Root_Stream_Type'Class;
      Data : in Ada.Streams.Stream_Element_Array);
      --  Apply the whole fiter stack on Data

   not overriding procedure Insert
     (Container : in out Stack;
      Element : in Filter'Class;
      On : in Side := Top);
      --  Insert Element in Container

   not overriding procedure Remove
     (Container : in out Stack;
      Element : in Filter'Class;
      From : in Side := Top);
      --  Remove the element of Container on the given side, checking it's
      --  equal to Element (otherwise Program_Error is raised).

private

   package Filter_Lists is new Ada.Containers.Indefinite_Doubly_Linked_Lists
     (Filter'Class);

   type Stack is new Filter with record
      Backend : Filter_Lists.List;
   end record;

end Natools.Web.Filters;
