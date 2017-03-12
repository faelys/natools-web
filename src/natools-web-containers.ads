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
-- Natools.Web.Containres provides common containers for all website-wide   --
-- persistent data.                                                         --
------------------------------------------------------------------------------

with Ada.Calendar.Time_Zones;
with Ada.Containers.Indefinite_Doubly_Linked_Lists;
with Ada.Streams;
with Natools.Constant_Indefinite_Ordered_Maps;
with Natools.References;
with Natools.S_Expressions.Atom_Refs;
with Natools.S_Expressions.Caches;
with Natools.S_Expressions.Lockable;
with Natools.Storage_Pools;

package Natools.Web.Containers is

   type Date is record
      Time : Ada.Calendar.Time;
      Offset : Ada.Calendar.Time_Zones.Time_Offset;
   end record;

   package Date_Maps is new Constant_Indefinite_Ordered_Maps
     (S_Expressions.Atom, Date, S_Expressions.Less_Than);

   procedure Set_Dates
     (Map : in out Date_Maps.Constant_Map;
      Date_List : in out S_Expressions.Lockable.Descriptor'Class);
      --  (Re)initialize date database with then given list


   package Expression_Maps is new Constant_Indefinite_Ordered_Maps
     (S_Expressions.Atom,
      S_Expressions.Caches.Cursor,
      S_Expressions.Less_Than,
      S_Expressions.Caches."=");

   procedure Set_Expressions
     (Map : in out Expression_Maps.Constant_Map;
      Expression_List : in out S_Expressions.Lockable.Descriptor'Class);
      --  (Re)initialize expression database with the given list


   package Expression_Map_Maps is new Constant_Indefinite_Ordered_Maps
     (S_Expressions.Atom,
      Expression_Maps.Constant_Map,
      S_Expressions.Less_Than,
      Expression_Maps."=");

   procedure Set_Expression_Maps
     (Map : in out Expression_Map_Maps.Constant_Map;
      Expression_Map_List : in out S_Expressions.Lockable.Descriptor'Class);
      --  (Re)initialize expression map database with the given list



   package Unsafe_Atom_Lists is
     new Ada.Containers.Indefinite_Doubly_Linked_Lists
        (S_Expressions.Atom, Ada.Streams."=");

   procedure Append_Atoms
     (Target : in out Unsafe_Atom_Lists.List;
      Expression : in out S_Expressions.Lockable.Descriptor'Class);


   type Atom_Array is array (S_Expressions.Count range <>)
     of S_Expressions.Atom_Refs.Immutable_Reference;

   package Atom_Array_Refs is new References
     (Atom_Array,
      Storage_Pools.Access_In_Default_Pool'Storage_Pool,
      Storage_Pools.Access_In_Default_Pool'Storage_Pool);

   function Create (Source : Unsafe_Atom_Lists.List)
     return Atom_Array_Refs.Immutable_Reference;

   function Create
     (Expression : in out S_Expressions.Lockable.Descriptor'Class)
     return Atom_Array_Refs.Immutable_Reference;

end Natools.Web.Containers;
