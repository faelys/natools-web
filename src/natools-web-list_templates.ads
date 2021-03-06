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
-- Natools.Web.List_Templates provides an abstraction around list rendering --
-- parameters, and a generic procedure to use it.                           --
-- When the list is empty, only If_Empty is output, otherwise the following --
-- scheme is rendered:                                                      --
-- <prefix> [<ellipsis_prefix>] <item_1> <separator> <item_2> ...           --
--    ... <separator> <item_n> [<ellipsis_suffix>] <suffix>                 --
-- The optional ellipses are output when the list is only partially output. --
-- Ellipses_Are_Items controls whether the non-empty ellipses count towards --
-- the given limit.                                                         --
-- Shown_End controls whether partially rendered lists omit items at the    --
-- beginning or at the end of the displayed list (i.e. after taking into    --
-- account Going).                                                          --
------------------------------------------------------------------------------

with Ada.Containers;
with Ada.Iterator_Interfaces;
with Natools.S_Expressions.Atom_Refs;
with Natools.S_Expressions.Caches;
with Natools.S_Expressions.Lockable;
with Natools.Web.Sites;

package Natools.Web.List_Templates is

   type Count is new Ada.Containers.Count_Type;

   type Direction is (Forward, Backward);

   type List_End is (Beginning, Ending);

   type Parameters is record
      Ellipses_Are_Items : Boolean := False;
      Ellipsis_Prefix : S_Expressions.Atom_Refs.Immutable_Reference;
      Ellipsis_Suffix : S_Expressions.Atom_Refs.Immutable_Reference;
      Going : Direction := Forward;
      Shown_End : List_End := Beginning;
      If_Empty : S_Expressions.Atom_Refs.Immutable_Reference;
      Limit : Count := 0;
      Prefix : S_Expressions.Atom_Refs.Immutable_Reference;
      Separator : S_Expressions.Atom_Refs.Immutable_Reference;
      Suffix : S_Expressions.Atom_Refs.Immutable_Reference;
      Template : S_Expressions.Caches.Cursor;
   end record;


   procedure Read_Parameters
     (Object : in out Parameters;
      Expression : in out S_Expressions.Lockable.Descriptor'Class);

   function Read_Parameters
     (Expression : in out S_Expressions.Lockable.Descriptor'Class)
     return Parameters;


   generic
      type Cursor (<>) is private;
      with package Iterators is new Ada.Iterator_Interfaces
        (Cursor => Cursor, others => <>);
      with procedure Render
        (Exchange : in out Sites.Exchange;
         Position : in Cursor;
         Expression : in out S_Expressions.Lockable.Descriptor'Class) is <>;
   procedure Render
     (Exchange : in out Sites.Exchange;
      Iterator : in Iterators.Reversible_Iterator'Class;
      Param : in Parameters);

end Natools.Web.List_Templates;
