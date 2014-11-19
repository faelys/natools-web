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
-- Natools.Web.Sites provides a container for data about a related set of   --
-- pages (i.e. a website).                                                  --
------------------------------------------------------------------------------

with Ada.Streams;
with Natools.Constant_Indefinite_Ordered_Maps;
with Natools.S_Expressions.Caches;
with Natools.S_Expressions.Lockable;
with Natools.Web.Containers;
with Natools.Web.Exchanges;

private with Natools.S_Expressions.Atom_Refs;

package Natools.Web.Sites is

   type Site is private;

   function Create (File_Name : String) return Site;
      --  Build a new object from the given file

   procedure Reload (Object : in out Site);
      --  Reload Object data from its original file

   procedure Respond
     (Object : not null access Site;
      Exchange : in out Exchanges.Exchange);
      --  Look up internal data to provide a response in Exchange


   procedure Get_Template
     (Object : in Site;
      Name : in S_Expressions.Atom;
      Template : out S_Expressions.Caches.Cursor;
      Found : out Boolean);
      --  Retrieve a template from its name

   function Default_Template (Object : Site) return S_Expressions.Atom;
      --  Retrieve the default template name


   type Page is interface;

   procedure Respond
     (Object : in out Page;
      Exchange : in out Exchanges.Exchange;
      Parent : not null access constant Site;
      Extra_Path : in S_Expressions.Atom)
     is abstract
     with Pre'Class => not Exchanges.Has_Response (Exchange);


   package Page_Maps is new Constant_Indefinite_Ordered_Maps
     (S_Expressions.Atom, Page'Class, Ada.Streams."<");


   type Visible is interface;

   procedure Render
     (Exchange : in out Exchanges.Exchange;
      Object : in Visible;
      Parent : not null access constant Site;
      Expression : in out S_Expressions.Lockable.Descriptor'Class)
     is abstract;


   package Visible_Maps is new Constant_Indefinite_Ordered_Maps
     (S_Expressions.Atom, Visible'Class, Ada.Streams."<");

private

   type Site is record
      Default_Template : S_Expressions.Atom_Refs.Immutable_Reference;
      File_Name : S_Expressions.Atom_Refs.Immutable_Reference;
      Pages : Page_Maps.Updatable_Map;
      Static : Containers.Atom_Array_Refs.Immutable_Reference;
      Templates : Containers.Expression_Maps.Constant_Map;
   end record;

   type Site_Builder is record
      Default_Template : S_Expressions.Atom_Refs.Immutable_Reference;
      File_Prefix : S_Expressions.Atom_Refs.Immutable_Reference;
      File_Suffix : S_Expressions.Atom_Refs.Immutable_Reference;
      Path_Prefix : S_Expressions.Atom_Refs.Immutable_Reference;
      Path_Suffix : S_Expressions.Atom_Refs.Immutable_Reference;
      Pages : Page_Maps.Unsafe_Maps.Map;
      Static : Containers.Unsafe_Atom_Lists.List;
      Templates : Containers.Expression_Maps.Constant_Map;
   end record;

   procedure Update
     (Builder : in out Site_Builder;
      Expression : in out S_Expressions.Lockable.Descriptor'Class);

end Natools.Web.Sites;
