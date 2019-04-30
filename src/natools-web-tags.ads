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
-- Natools.Web.Tags provides a hierarchical tag mechanism, including key    --
-- subsets viewed as page lists and rendering subprograms.                  --
------------------------------------------------------------------------------

with Natools.Constant_Indefinite_Ordered_Maps;
with Natools.S_Expressions.Atom_Refs;
with Natools.S_Expressions.Lockable;
with Natools.S_Expressions.Printers;
with Natools.Web.Containers;

limited with Natools.Web.Sites;

private with Ada.Containers.Indefinite_Ordered_Maps;
private with Ada.Iterator_Interfaces;
private with Natools.References;
private with Natools.Storage_Pools;

package Natools.Web.Tags is

   type Visible is interface;

   type Visible_Access is access constant Visible'Class;
      --  For weak backward references, use with caution

   procedure Render
     (Exchange : in out Sites.Exchange;
      Object : in Visible;
      Expression : in out S_Expressions.Lockable.Descriptor'Class)
     is abstract;


   type Tag_List is private;

   Empty_Tag_List : constant Tag_List;

   procedure Clear (List : in out Tag_List);
      --  Delete everything in List

   function Create
     (Tag_Names : Containers.Atom_Array;
      Common_Key : S_Expressions.Atom_Refs.Immutable_Reference)
     return Tag_List;
      --  Create a tag list using all the names associated with the same key

   procedure Append
     (List : in out Tag_List;
      Expression : in out S_Expressions.Lockable.Descriptor'Class);
      --  Read Expression to extract tags to append to List

   procedure Print
     (List : in Tag_List;
      Printer : in out S_Expressions.Printers.Printer'Class);
      --  Serialize the tag list into the given printer


   type Tag_DB_Builder is private;

   procedure Register
     (Builder : in out Tag_DB_Builder;
      Keys : in Tag_List;
      Element : in Visible'Class);
      --  Register Element with the given Keys into Builder


   type Tag_DB is private;

   function Create (Builder : Tag_DB_Builder) return Tag_DB;

   procedure Live_Register
     (DB : in out Tag_DB;
      Keys : in Tag_List;
      Element : in Visible'Class);
      --  Inefficiently register Element with the given Keys into DB

   procedure Live_Unregister
     (DB : in out Tag_DB;
      Keys : in Tag_List;
      Element : in Visible'Class);
      --  Inefficiently unregister Element with the given Keys from DB

   procedure Render
     (Exchange : in out Sites.Exchange;
      DB : in Tag_DB;
      Expression : in out S_Expressions.Lockable.Descriptor'Class;
      Caller_Tags : in Tag_List := Empty_Tag_List;
      Optional : in Boolean := False);
      --  Render Expression as 'tag_name tag_template' or a list of such expr

   procedure Render
     (Exchange : in out Sites.Exchange;
      List : in Tag_List;
      DB : in Tag_DB;
      Prefix : in S_Expressions.Atom;
      Expression : in out S_Expressions.Lockable.Descriptor'Class);
      --  Render a list of tags composed of the elements in List whose names
      --  start with Prefix.

   procedure Reset
     (DB : in out Tag_DB;
      Builder : in Tag_DB_Builder);
      --  Replace DB with the contents accumulated in Builder


   type Tag_Contents is private;

   function Get_Tag
     (DB : in Tag_DB;
      Name : in S_Expressions.Atom;
      Caller_Tags : in Tag_List := Empty_Tag_List)
     return Tag_Contents;
      --  Obtain tags contents from its name.
      --  Return an empty object when there is no tag with Name.

   function Has_Element (Tag : Tag_Contents) return Boolean;
   function Is_Empty (Tag : Tag_Contents) return Boolean;
      --  Return whether Tag is empty or has an element

   procedure Render
     (Exchange : in out Sites.Exchange;
      Tag : in Tag_Contents;
      Expression : in out S_Expressions.Lockable.Descriptor'Class);
      --  Render the given Tag into Exchange

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

   function Create (Builder : in Tag_DB_Builder) return Tag_Maps.Constant_Map;

   type Tag_DB is record
      Internal : Tag_Maps.Constant_Map;
   end record;

   type Tag_Contents is record
      DB : Tag_DB;
      Caller_Tags : Tag_List;
      Position : Tag_Maps.Cursor;
   end record;

   function Current_Element (Tag : Tag_Contents) return Page_Maps.Cursor;

   function Has_Element (Tag : Tag_Contents) return Boolean
     is (Tag_Maps.Has_Element (Tag.Position));

   function Is_Empty (Tag : Tag_Contents) return Boolean
     is (not Tag_Maps.Has_Element (Tag.Position));


   type Tag_List_Cursor is record
      DB : Tag_DB;
      List : Tag_Lists.Immutable_Reference;
      Index : Natural := 0;
   end record;

   function Has_Element (Position : Tag_List_Cursor) return Boolean
     is ((not Position.List.Is_Empty)
         and then Position.Index in Position.List.Query.Data'Range);

   function No_Element return Tag_List_Cursor
     is ((DB => <>,
          List => Tag_Lists.Null_Immutable_Reference,
          Index => 0));

   procedure Next (Position : in out Tag_List_Cursor);

   procedure Previous (Position : in out Tag_List_Cursor);

   procedure Render
     (Exchange : in out Sites.Exchange;
      Position : in Tag_List_Cursor;
      Expression : in out S_Expressions.Lockable.Descriptor'Class);

   package Tag_List_Iterators is new Ada.Iterator_Interfaces
     (Tag_List_Cursor, Has_Element);

   type Tag_List_Iterator is new Tag_List_Iterators.Reversible_Iterator
   with record
      DB : Tag_DB;
      List : Tag_Lists.Immutable_Reference;
      Prefix : S_Expressions.Atom_Refs.Immutable_Reference;
   end record;

   overriding function First (Iterator : Tag_List_Iterator)
     return Tag_List_Cursor;

   overriding function Last (Iterator : Tag_List_Iterator)
     return Tag_List_Cursor;

   overriding function Next
     (Iterator : Tag_List_Iterator;
      Position : Tag_List_Cursor)
     return Tag_List_Cursor;

   overriding function Previous
     (Iterator : Tag_List_Iterator;
      Position : Tag_List_Cursor)
     return Tag_List_Cursor;


   type Recursion_Kind is (Children, Descendants, Leaves);

   package Tag_Iterators is new Ada.Iterator_Interfaces
     (Tag_Contents, Has_Element);

   type Tag_Iterator is new Tag_Iterators.Reversible_Iterator with record
      DB : Tag_DB;
      Caller_Tags : Tag_List;
      First : Tag_Maps.Cursor;
      Last : Tag_Maps.Cursor;
      Prefix_Length : S_Expressions.Count;
      Recursion : Recursion_Kind;
   end record;

   overriding function First (Iterator : Tag_Iterator) return Tag_Contents;

   overriding function Last (Iterator : Tag_Iterator) return Tag_Contents;

   overriding function Next
     (Iterator : Tag_Iterator; Position : Tag_Contents) return Tag_Contents;

   overriding function Previous
     (Iterator : Tag_Iterator; Position : Tag_Contents) return Tag_Contents;



   Empty_Description : constant Tag_Description := (Tag | Key => <>);

   Empty_Tag_List : constant Tag_List := (Internal => <>);

end Natools.Web.Tags;
