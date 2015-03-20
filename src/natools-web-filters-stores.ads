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
-- Natools.Web.Filters.Stores provides a database of filter constructors    --
-- and instances.                                                           --
------------------------------------------------------------------------------

with Natools.S_Expressions.Lockable;

private with Natools.Constant_Indefinite_Ordered_Maps;

package Natools.Web.Filters.Stores is
   pragma Preelaborate;

   No_Filter : exception;
      --  Raised by a constructor when unable to return a valid filter
      --  or by Get_Filter called with an unknown name.

   type Constructor is not null access function
     (Arguments : in out S_Expressions.Lockable.Descriptor'Class)
     return Filter'Class;
      --  Build a filter using Arguments

   type Store is tagged private;

   function Duplicate (Source : in Store) return Store;
      --  Return a new store, copying the constructor database but with
      --  a clean filter database.

   function Get_Filter
     (Container : in Store;
      Name : in S_Expressions.Atom)
     return Filter'Class;
      --  Return the filter whose name is given, or raise No_Filter

   procedure Populate
     (Container : in out Store;
      Expression : in out S_Expressions.Lockable.Descriptor'Class);
      --  Populate the filter database using expression and the constructors

   procedure Register
     (Container : in out Store;
      Name : in S_Expressions.Atom;
      Callback : in Constructor);
      --  Register a container into Container

   procedure Register
     (Container : in out Store;
      Name : in S_Expressions.Atom;
      Filter : in Filters.Filter'Class);
      --  Register a filter into Container

private

   package Constructor_Maps is new Constant_Indefinite_Ordered_Maps
     (S_Expressions.Atom, Constructor, S_Expressions.Less_Than);

   package Filter_Maps is new Constant_Indefinite_Ordered_Maps
     (S_Expressions.Atom, Filter'Class, S_Expressions.Less_Than);

   type Store is tagged record
      Constructors : Constructor_Maps.Constant_Map;
      Filters : Filter_Maps.Constant_Map;
   end record;

end Natools.Web.Filters.Stores;
