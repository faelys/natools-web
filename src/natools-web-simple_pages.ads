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
-- Natools.Web.Simple_Pages provides a simple implementation of Sites.Page  --
-- and Tags.Visible, representing a single page in a usual blog-style       --
-- static site.                                                             --
------------------------------------------------------------------------------

with Ada.Calendar;
with Natools.S_Expressions.Atom_Refs;
with Natools.S_Expressions.Lockable;
with Natools.Web.Sites;
with Natools.Web.Tags;

private with Natools.References;
private with Natools.S_Expressions.Caches;
private with Natools.Storage_Pools;
private with Natools.Web.Containers;
private with Natools.Web.Comments;
private with Natools.Web.String_Tables;

package Natools.Web.Simple_Pages is

   type Page_Template is private;

   procedure Set_Comments
     (Object : in out Page_Template;
      Expression : in out S_Expressions.Lockable.Descriptor'Class);

   procedure Set_Comment_Path_Prefix
     (Object : in out Page_Template;
      Prefix : in S_Expressions.Atom);

   procedure Set_Comment_Path_Suffix
     (Object : in out Page_Template;
      Suffix : in S_Expressions.Atom);

   procedure Set_Elements
     (Object : in out Page_Template;
      Expression : in out S_Expressions.Lockable.Descriptor'Class);

   procedure Set_Component
     (Object : in out Page_Template;
      Name : in S_Expressions.Atom;
      Arguments : in out S_Expressions.Lockable.Descriptor'Class;
      Known_Component : out Boolean);

   procedure Update
     (Object : in out Page_Template;
      Expression : in out S_Expressions.Lockable.Descriptor'Class);

   Default_Template : constant Page_Template;


   type Page_Ref is new Tags.Visible and Sites.Page with private;

   function Create
     (File_Path, Web_Path : in S_Expressions.Atom_Refs.Immutable_Reference;
      Template : in Page_Template := Default_Template;
      Name : in S_Expressions.Atom := S_Expressions.Null_Atom)
     return Page_Ref;

   function Create
     (Expression : in out S_Expressions.Lockable.Descriptor'Class;
      Template : in Page_Template := Default_Template;
      Name : in S_Expressions.Atom := S_Expressions.Null_Atom)
     return Page_Ref;

   procedure Get_Lifetime
     (Page : in Page_Ref;
      Publication : out Ada.Calendar.Time;
      Has_Publication : out Boolean;
      Expiration : out Ada.Calendar.Time;
      Has_Expiration : out Boolean);

   function Get_Tags (Page : Page_Ref) return Tags.Tag_List;

   procedure Register
     (Page : in Page_Ref;
      Builder : in out Sites.Site_Builder;
      Path : in S_Expressions.Atom);

   overriding procedure Render
     (Exchange : in out Sites.Exchange;
      Object : in Page_Ref;
      Expression : in out S_Expressions.Lockable.Descriptor'Class);

   overriding procedure Respond
     (Object : in out Page_Ref;
      Exchange : in out Sites.Exchange;
      Extra_Path : in S_Expressions.Atom);


   type Loader is new Sites.Page_Loader with private;

   overriding procedure Load
     (Object : in out Loader;
      Builder : in out Sites.Site_Builder;
      Path : in S_Expressions.Atom);

   function Create (File : in S_Expressions.Atom)
     return Sites.Page_Loader'Class;

   procedure Register_Loader (Site : in out Sites.Site);

private

   type Page_Template is record
      Comments : Containers.Optional_Expression;
      Comment_Path_Prefix : S_Expressions.Atom_Refs.Immutable_Reference;
      Comment_Path_Suffix : S_Expressions.Atom_Refs.Immutable_Reference;
      Elements : Containers.Expression_Maps.Constant_Map;
      Name : S_Expressions.Atom_Refs.Immutable_Reference;
   end record;

   Default_Template : constant Page_Template := (others => <>);

   type Page_Data is new Tags.Visible with record
      Self : Tags.Visible_Access;
      File_Path : S_Expressions.Atom_Refs.Immutable_Reference;
      Web_Path : S_Expressions.Atom_Refs.Immutable_Reference;
      Elements : Containers.Expression_Maps.Constant_Map;
      Tags : Web.Tags.Tag_List;
      Dates : Containers.Date_Maps.Constant_Map;
      Comment_List : Comments.Comment_List;
      Maps : String_Tables.String_Table_Map;
   end record;

   not overriding procedure Get_Element
     (Data : in Page_Data;
      Name : in S_Expressions.Atom;
      Element : out S_Expressions.Caches.Cursor;
      Found : out Boolean);

   overriding procedure Render
     (Exchange : in out Sites.Exchange;
      Object : in Page_Data;
      Expression : in out S_Expressions.Lockable.Descriptor'Class);


   package Data_Refs is new References
     (Page_Data,
      Storage_Pools.Access_In_Default_Pool'Storage_Pool,
      Storage_Pools.Access_In_Default_Pool'Storage_Pool);

   type Page_Ref is new Tags.Visible and Sites.Page with record
      Ref : Data_Refs.Reference;
   end record;

   function Get_Tags (Page : Page_Ref) return Tags.Tag_List
     is (Page.Ref.Query.Data.Tags);


   type Loader is new Sites.Page_Loader with record
      File_Path : S_Expressions.Atom_Refs.Immutable_Reference;
   end record;

end Natools.Web.Simple_Pages;
