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
-- Natools.Web.String_Tables provides a container and rendering primitives  --
-- for two-dimensional collections of strings (actually s-expression        --
-- atoms).                                                                  --
------------------------------------------------------------------------------

with Natools.S_Expressions;
with Natools.S_Expressions.Lockable;
with Natools.Web.Sites;
with Natools.Web.Tags;

private with Ada.Iterator_Interfaces;
private with Natools.References;
private with Natools.Storage_Pools;
private with Natools.Constant_Indefinite_Ordered_Maps;
private with Natools.Web.Containers;

package Natools.Web.String_Tables is

   type String_Table is new Tags.Visible with private;

   not overriding function Create
     (Expression : in out S_Expressions.Lockable.Descriptor'Class)
     return String_Table;

   overriding procedure Render
     (Exchange : in out Sites.Exchange;
      Object : in String_Table;
      Expression : in out S_Expressions.Lockable.Descriptor'Class);


   type String_Table_Map is new Tags.Visible with private;

   not overriding function Create
     (Expression : in out S_Expressions.Lockable.Descriptor'Class)
     return String_Table_Map;

   overriding procedure Render
     (Exchange : in out Sites.Exchange;
      Object : in String_Table_Map;
      Expression : in out S_Expressions.Lockable.Descriptor'Class);

private

   use type S_Expressions.Offset;

   type Table is array (S_Expressions.Offset range <>)
     of Containers.Atom_Array_Refs.Immutable_Reference;

   package Table_References is new Natools.References
     (Table,
      Natools.Storage_Pools.Access_In_Default_Pool'Storage_Pool,
      Natools.Storage_Pools.Access_In_Default_Pool'Storage_Pool);


   function Create
     (Expression : in out S_Expressions.Lockable.Descriptor'Class)
     return Table_References.Immutable_Reference;


   type Cursor is record
      Ref : Table_References.Immutable_Reference;
      Index : S_Expressions.Offset;
   end record;

   function Has_Element (Position : in Cursor) return Boolean
     is (Position.Index in Position.Ref.Query.Data.all'Range);

   procedure Render
     (Exchange : in out Sites.Exchange;
      Position : in Cursor;
      Expression : in out S_Expressions.Lockable.Descriptor'Class);

   package Iterator_Interfaces is new Ada.Iterator_Interfaces
     (Cursor, Has_Element);

   type Table_Iterator is new Iterator_Interfaces.Reversible_Iterator
   with record
      Ref : Table_References.Immutable_Reference;
   end record;

   overriding function First (Object : in Table_Iterator)
     return Cursor;

   overriding function Last (Object : in Table_Iterator)
     return Cursor;

   overriding function Next
     (Object : in Table_Iterator;
      Position : in Cursor)
     return Cursor;

   overriding function Previous
     (Object : in Table_Iterator;
      Position : in Cursor)
     return Cursor;


   type String_Table is new Tags.Visible with record
      Ref : Table_References.Immutable_Reference;
   end record;

   package Table_Maps is new Constant_Indefinite_Ordered_Maps
     (S_Expressions.Atom, Table_References.Immutable_Reference,
      S_Expressions."<", Table_References."=");

   type String_Table_Map is new Tags.Visible with record
      Map : Table_Maps.Constant_Map;
   end record;

end Natools.Web.String_Tables;
