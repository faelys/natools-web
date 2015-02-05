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
-- Natools.Web.Comments provides an implementation of user-posted comments  --
-- like is commonly found on blogs.                                         --
------------------------------------------------------------------------------

with Natools.S_Expressions.Atom_Refs;
with Natools.S_Expressions.Lockable;
with Natools.S_Expressions.Printers;
with Natools.Web.Sites;
with Natools.Web.Tags;

private with Ada.Calendar;
private with Ada.Finalization;
private with Ada.Iterator_Interfaces;
private with Natools.References;
private with Natools.Storage_Pools;
private with Natools.Web.Containers;

package Natools.Web.Comments is

   type Comment_Ref is new Tags.Visible with private;

   overriding procedure Render
     (Exchange : in out Sites.Exchange;
      Object : in Comment_Ref;
      Expression : in out S_Expressions.Lockable.Descriptor'Class);
      --  Render a comment


   type Comment_List is tagged private;

   procedure Load
     (Object : in out Comment_List;
      Builder : in out Sites.Site_Builder;
      Parent : in Tags.Visible_Access := null;
      Parent_Path : in S_Expressions.Atom_Refs.Immutable_Reference
        := S_Expressions.Atom_Refs.Null_Immutable_Reference);
      --  Load comment list from Builder back-end and register all comments

   procedure Render
     (Exchange : in out Sites.Exchange;
      Object : in Comment_List;
      Expression : in out S_Expressions.Lockable.Descriptor'Class);
      --  Render a comment list

   procedure Respond
     (List : in out Comment_List;
      Exchange : in out Sites.Exchange;
      Extra_Path : in S_Expressions.Atom);
      --  Respond to a request for the comment list

   procedure Set
     (List : out Comment_List;
      Expression : in out S_Expressions.Lockable.Descriptor'Class);
      --  (Re)initialize List using data from Expression

private

   type Comment_Data is record
      Date : Ada.Calendar.Time;
      Id : S_Expressions.Atom_Refs.Immutable_Reference;
      Name : S_Expressions.Atom_Refs.Immutable_Reference;
      Mail : S_Expressions.Atom_Refs.Immutable_Reference;
      Link : S_Expressions.Atom_Refs.Immutable_Reference;
      Text : S_Expressions.Atom_Refs.Immutable_Reference;
   end record;

   procedure Write
     (Comment : in Comment_Data;
      Output : in out S_Expressions.Printers.Printer'Class);
      --  Serialize a comment into the given S_Expression stream


   type Comment_Array is array (S_Expressions.Offset range <>) of Comment_Data;

   type Comment_Container (Size : S_Expressions.Count) is record
      Data : Comment_Array (1 .. Size);
      Parent : Tags.Visible_Access;
   end record;

   package Comment_Array_Refs is new References
     (Comment_Container,
      Storage_Pools.Access_In_Default_Pool'Storage_Pool,
      Storage_Pools.Access_In_Default_Pool'Storage_Pool);


   type Comment_Ref is new Tags.Visible with record
      List : Comment_Array_Refs.Reference;
      Position : S_Expressions.Offset;
   end record;


   type Comment_List is new Ada.Finalization.Controlled with record
      Backend_Name : S_Expressions.Atom_Refs.Immutable_Reference;
      Backend_Path : S_Expressions.Atom_Refs.Immutable_Reference;
      Parent_Path : S_Expressions.Atom_Refs.Immutable_Reference;
      Comments : Comment_Array_Refs.Reference;
      Tags : Containers.Atom_Array_Refs.Immutable_Reference;
   end record;

   overriding procedure Finalize (Object : in out Comment_List);


   type Cursor is record
      Value : Comment_Ref;
   end record;

   function Has_Element (Position : Cursor) return Boolean
     is (not Position.Value.List.Is_Empty
         and then Position.Value.Position
               in Position.Value.List.Query.Data.Data'Range);

   procedure Render
     (Exchange : in out Sites.Exchange;
      Position : in Cursor;
      Expression : in out S_Expressions.Lockable.Descriptor'Class);

   package Comment_Iterators is new Ada.Iterator_Interfaces
     (Cursor, Has_Element);

   type Comment_Range is new Comment_Iterators.Reversible_Iterator with record
      List : Comment_Array_Refs.Reference;
   end record;

   overriding function First (Object : Comment_Range) return Cursor;
   overriding function Last (Object : Comment_Range) return Cursor;
   overriding function Next (Object : Comment_Range; Position : Cursor)
     return Cursor;
   overriding function Previous (Object : Comment_Range; Position : Cursor)
     return Cursor;


   No_Comment : constant Comment_Ref
     := (List => Comment_Array_Refs.Null_Reference,
         Position => 0);

   Empty_List : constant Comment_List
     := (Ada.Finalization.Controlled with
         Backend_Name => S_Expressions.Atom_Refs.Null_Immutable_Reference,
         Backend_Path => S_Expressions.Atom_Refs.Null_Immutable_Reference,
         Parent_Path => S_Expressions.Atom_Refs.Null_Immutable_Reference,
         Comments => Comment_Array_Refs.Null_Reference,
         Tags => Containers.Atom_Array_Refs.Null_Immutable_Reference);

end Natools.Web.Comments;
