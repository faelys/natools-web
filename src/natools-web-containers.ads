------------------------------------------------------------------------------
-- Copyright (c) 2014-2019, Natacha Porté                                   --
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
with Ada.Containers.Doubly_Linked_Lists;
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

   procedure Add_Expressions
     (Map : in out Expression_Maps.Constant_Map;
      Expression_List : in out S_Expressions.Lockable.Descriptor'Class);
      --  Add expressions from the given list to a database

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


   type Optional_Expression (Is_Empty : Boolean := True) is record
      case Is_Empty is
         when True  => null;
         when False => Value : S_Expressions.Caches.Cursor;
      end case;
   end record;


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



   package Atom_Row_Lists is new Ada.Containers.Doubly_Linked_Lists
     (Atom_Array_Refs.Immutable_Reference,
      Atom_Array_Refs."=");

   type Atom_Table is array (S_Expressions.Offset range <>)
     of Atom_Array_Refs.Immutable_Reference;

   package Atom_Table_Refs is new References
     (Atom_Table,
      Natools.Storage_Pools.Access_In_Default_Pool'Storage_Pool,
      Natools.Storage_Pools.Access_In_Default_Pool'Storage_Pool);

   function Create
     (Expression : in out S_Expressions.Lockable.Descriptor'Class)
     return Atom_Table_Refs.Immutable_Reference;

   function Create
     (Row_List : in Atom_Row_Lists.List)
     return Atom_Table_Refs.Immutable_Reference;



   type Atom_Set is private;

   Null_Atom_Set : constant Atom_Set;

   function Create (Source : in Atom_Array) return Atom_Set;
   function Create (Source : in Unsafe_Atom_Lists.List) return Atom_Set;

   function Contains
     (Set : in Atom_Set;
      Value : in S_Expressions.Atom)
     return Boolean;
   function Contains
     (Set : in Atom_Set;
      Value : in String)
     return Boolean;

   function Elements (Set : in Atom_Set)
     return Atom_Array_Refs.Immutable_Reference;


   type Identity is record
      User : S_Expressions.Atom_Refs.Immutable_Reference;
      Groups : Atom_Set;
   end record;

   Null_Identity : constant Identity;

private

   type Atom_Set is record
      Elements : Atom_Array_Refs.Immutable_Reference;
   end record;

   function Elements (Set : in Atom_Set)
     return Atom_Array_Refs.Immutable_Reference
     is (Set.Elements);

   Null_Atom_Set : constant Atom_Set
     := (Elements => Atom_Array_Refs.Null_Immutable_Reference);

   Null_Identity : constant Identity
     := (User => S_Expressions.Atom_Refs.Null_Immutable_Reference,
         Groups => Null_Atom_Set);

end Natools.Web.Containers;
