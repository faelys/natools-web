------------------------------------------------------------------------------
-- Copyright (c) 2015-2019, Natacha Porté                                   --
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

private with Ada.Calendar.Time_Zones;
private with Ada.Containers.Doubly_Linked_Lists;
private with Ada.Finalization;
private with Natools.Constant_Indefinite_Ordered_Maps;
private with Natools.References;
private with Natools.Storage_Pools;
private with Natools.Web.Containers;
private with Natools.Web.Sites.Updates;

package Natools.Web.Comments is

   type Comment_Ref is new Tags.Visible with private;

   overriding procedure Render
     (Exchange : in out Sites.Exchange;
      Object : in Comment_Ref;
      Expression : in out S_Expressions.Lockable.Descriptor'Class);
      --  Render a comment


   type Comment_List is tagged private;

   function Is_Null (Object : in Comment_List) return Boolean;
      --  Return whether the list is usable (even if empty)

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

   package Comment_Atoms is
      type Enum is (Name, Mail, Link, Text, Class, Note, Title, Filter);
      type Set is array (Enum) of S_Expressions.Atom_Refs.Immutable_Reference;
   end Comment_Atoms;

   package Comment_Flags is
      type Enum is (Preprocessed, Ignored);
      type Set is array (Enum) of Boolean with Pack;
   end Comment_Flags;

   package List_Flags is
      type Enum is
        (Ignore_By_Default, Allow_Date_Override, Closed, Allow_Ignore);
      type Set is array (Enum) of Boolean with Pack;
   end List_Flags;

   type Comment_Data is record
      Date : Ada.Calendar.Time;
      Offset : Ada.Calendar.Time_Zones.Time_Offset;
      Id : S_Expressions.Atom_Refs.Immutable_Reference;
      Atoms : Comment_Atoms.Set;
      Flags : Comment_Flags.Set := (others => False);
      Parent : Tags.Visible_Access;
      Rank : Positive;
   end record;

   procedure Preprocess
     (Comment : in out Comment_Data;
      List : in Comment_List;
      Site : in Sites.Site);
   procedure Preprocess
     (Comment : in out Comment_Data;
      List : in Comment_List;
      Builder : in Sites.Site_Builder);
      --  Preprocess comment field contents

   procedure Write
     (Comment : in Comment_Data;
      Output : in out S_Expressions.Printers.Printer'Class);
      --  Serialize a comment into the given S_Expression stream


   package Comment_Maps is new Natools.Constant_Indefinite_Ordered_Maps
     (S_Expressions.Atom, Comment_Data, S_Expressions."<");

   procedure Set_Parent
     (Container : in out Comment_Maps.Updatable_Map;
      Parent : in Tags.Visible_Access);

   procedure Update_Ranks (Container : in out Comment_Maps.Updatable_Map);


   protected type Comment_Container is
      procedure Initialize
        (Data : in Comment_Maps.Unsafe_Maps.Map;
         Parent : in Tags.Visible_Access);

      procedure Insert (Data : in Comment_Data);

      procedure Ignore
        (Id : in S_Expressions.Atom;
         Ref : out S_Expressions.Atom_Refs.Immutable_Reference);

      procedure Orphan;

      function Find (Id : S_Expressions.Atom_Refs.Immutable_Reference)
        return Comment_Maps.Cursor;

      function First return Comment_Maps.Cursor;

      function Iterate
        return Comment_Maps.Map_Iterator_Interfaces.Reversible_Iterator'Class;

      function Length return Natural;
   private
      Map : Comment_Maps.Updatable_Map;
      Parent : Tags.Visible_Access;
   end Comment_Container;

   package Container_Refs is new References
     (Comment_Container,
      Storage_Pools.Access_In_Default_Pool'Storage_Pool,
      Storage_Pools.Access_In_Default_Pool'Storage_Pool);


   type Comment_Ref is new Tags.Visible with record
      Container : Container_Refs.Reference;
      Id : S_Expressions.Atom_Refs.Immutable_Reference;
   end record;


   type Comment_List is new Ada.Finalization.Controlled with record
      Backend_Name : S_Expressions.Atom_Refs.Immutable_Reference;
      Backend_Path : S_Expressions.Atom_Refs.Immutable_Reference;
      Parent_Path : S_Expressions.Atom_Refs.Immutable_Reference;
      Comments : Container_Refs.Reference;
      Post_Filter : S_Expressions.Atom_Refs.Immutable_Reference;
      Tags : Containers.Atom_Array_Refs.Immutable_Reference;
      Text_Filters : Containers.Atom_Array_Refs.Immutable_Reference;
      Default_Text_Filter : S_Expressions.Atom_Refs.Immutable_Reference;
      Flags : List_Flags.Set := (others => False);
      Parent : Web.Tags.Visible_Access;
   end record;

   overriding procedure Finalize (Object : in out Comment_List);

   function Is_Null (Object : in Comment_List) return Boolean
     is (Object.Backend_Name.Is_Empty or else Object.Backend_Path.Is_Empty);


   type Comment_Inserter is new Sites.Updates.Site_Update with record
      Container : Container_Refs.Reference;
      Id : S_Expressions.Atom_Refs.Immutable_Reference;
      Tags : Containers.Atom_Array_Refs.Immutable_Reference;
   end record;

   overriding procedure Update
     (Self : in Comment_Inserter;
      Site : in out Sites.Site);


   package Id_Lists is new Ada.Containers.Doubly_Linked_Lists
     (S_Expressions.Atom_Refs.Immutable_Reference,
      S_Expressions.Atom_Refs."=");

   type Comment_Remover is new Sites.Updates.Site_Update with record
      Container : Container_Refs.Reference;
      Ids : Id_Lists.List;
      Tags : Containers.Atom_Array_Refs.Immutable_Reference;
   end record;

   overriding procedure Update
     (Self : in Comment_Remover;
      Site : in out Sites.Site);


   Empty_List : constant Comment_List
     := (Ada.Finalization.Controlled with
         Backend_Name => S_Expressions.Atom_Refs.Null_Immutable_Reference,
         Backend_Path => S_Expressions.Atom_Refs.Null_Immutable_Reference,
         Parent_Path => S_Expressions.Atom_Refs.Null_Immutable_Reference,
         Comments => Container_Refs.Null_Reference,
         Parent => null,
         Post_Filter => S_Expressions.Atom_Refs.Null_Immutable_Reference,
         Tags => Containers.Atom_Array_Refs.Null_Immutable_Reference,
         Text_Filters => Containers.Atom_Array_Refs.Null_Immutable_Reference,
         Default_Text_Filter
           => S_Expressions.Atom_Refs.Null_Immutable_Reference,
         Flags => (others => False));

end Natools.Web.Comments;
