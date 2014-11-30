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
-- Natools.Web.Tags provides a hierarchical tag mechanism, including key    --
-- subsets viewed as page lists and rendering subprograms.                  --
------------------------------------------------------------------------------

with Natools.Constant_Indefinite_Ordered_Maps;
with Natools.S_Expressions.Atom_Refs;
with Natools.S_Expressions.Lockable;
with Natools.Web.Exchanges;

private with Ada.Containers.Indefinite_Ordered_Maps;
private with Natools.References;
private with Natools.Storage_Pools;

package Natools.Web.Tags is

   type Visible is interface;

   procedure Render
     (Exchange : in out Exchanges.Exchange;
      Object : in Visible;
      Expression : in out S_Expressions.Lockable.Descriptor'Class)
     is abstract;


   type Tag_List is private;

   Empty_Tag_List : constant Tag_List;

   procedure Clear (List : in out Tag_List);
      --  Delete everything in List

   procedure Append
     (List : in out Tag_List;
      Expression : in out S_Expressions.Lockable.Descriptor'Class);
      --  Read Expression to extract tags to append to List


   type Tag_DB_Builder is private;

   procedure Register
     (Builder : in out Tag_DB_Builder;
      Keys : in Tag_List;
      Element : in Visible'Class);
      --  Register Element with the given Keys into Builder


   type Tag_DB is private;

   function Create (Builder : Tag_DB_Builder) return Tag_DB;

   procedure Reset
     (DB : in out Tag_DB;
      Builder : in Tag_DB_Builder);
      --  Replace DB with the contents accumulated in Builder


   type Tag_Contents is private;

   function Get_Tag
     (DB : in Tag_DB;
      Name : in S_Expressions.Atom;
      Parent_Tags : in Tag_List := Empty_Tag_List)
     return Tag_Contents;
      --  Obtain tags contents from its name.
      --  Return an empty object when there is no tag with Name.

   function Is_Empty (Tag : Tag_Contents) return Boolean;
      --  Return whether Tag is empty

private

   package Page_Maps is new Constant_Indefinite_Ordered_Maps
     (S_Expressions.Atom, Visible'Class, S_Expressions."<");


   package Tag_Maps is new Constant_Indefinite_Ordered_Maps
     (S_Expressions.Atom, Page_Maps.Constant_Map,
      S_Expressions."<", Page_Maps."=");


   type Tag_Description is record
      Tag : S_Expressions.Atom_Refs.Immutable_Reference;
      Key : S_Expressions.Atom_Refs.Immutable_Reference;
   end record;

   type Tag_Array is array (Positive range <>) of Tag_Description;

   package Tag_Lists is new References
     (Tag_Array,
      Storage_Pools.Access_In_Default_Pool'Storage_Pool,
      Storage_Pools.Access_In_Default_Pool'Storage_Pool);

   type Tag_List is record
      Internal : Tag_Lists.Immutable_Reference;
   end record;


   package Builder_Maps is new Ada.Containers.Indefinite_Ordered_Maps
     (S_Expressions.Atom, Page_Maps.Unsafe_Maps.Map,
      S_Expressions.Less_Than, Page_Maps.Unsafe_Maps."=");

   type Tag_DB_Builder is record
      Internal : Builder_Maps.Map;
   end record;

   procedure Add_Entry
     (Builder : in out Tag_DB_Builder;
      Tag : in S_Expressions.Atom;
      Key : in S_Expressions.Atom;
      Element : in Visible'Class);
      --  Add a single Element to the map designated by Tag

   procedure Add_To_Tag
     (Builder : in out Tag_DB_Builder;
      Tag : in S_Expressions.Atom;
      Key : in S_Expressions.Atom;
      Element : in Visible'Class);
      --  Add Element to Tag and to all its parents

   function Create (Builder : in Tag_DB_Builder) return Tag_Maps.Constant_Map;

   type Tag_DB is record
      Internal : Tag_Maps.Constant_Map;
   end record;

   type Tag_Contents is record
      Parent : Tag_Description;
      Position : Tag_Maps.Cursor;
   end record;

   function Is_Empty (Tag : Tag_Contents) return Boolean
     is (not Tag_Maps.Has_Element (Tag.Position));

   Empty_Description : constant Tag_Description := (Tag | Key => <>);

   Empty_Tag_List : constant Tag_List := (Internal => <>);

end Natools.Web.Tags;
