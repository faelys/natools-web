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

with Natools.Web.Containers;
with Natools.S_Expressions.Caches;

private with Natools.S_Expressions.Atom_Refs;
private with Natools.S_Expressions.Lockable;
private with Natools.Web.Page_Maps;

package Natools.Web.Sites is

   type Site is tagged private;

   function Create (File_Name : String) return Site;
      --  Build a new object from the given file

   procedure Reload (Object : in out Site);
      --  Reload Object data from its original file


   function Template
     (Object : Site;
      Name : S_Expressions.Atom)
     return S_Expressions.Caches.Cursor;
      --  Retrieve a template from its name

   function Default_Template (Object : Site) return S_Expressions.Atom;
      --  Retrieve the default template name

private

   type Site is tagged record
      Default_Template : S_Expressions.Atom_Refs.Immutable_Reference;
      File_Name : S_Expressions.Atom_Refs.Immutable_Reference;
      Pages : Page_Maps.Raw_Maps.Constant_Map;
      Templates : Containers.Expression_Maps.Constant_Map;
   end record;

   type Site_Builder is record
      Default_Template : S_Expressions.Atom_Refs.Immutable_Reference;
      File_Prefix : S_Expressions.Atom_Refs.Immutable_Reference;
      File_Suffix : S_Expressions.Atom_Refs.Immutable_Reference;
      Path_Prefix : S_Expressions.Atom_Refs.Immutable_Reference;
      Path_Suffix : S_Expressions.Atom_Refs.Immutable_Reference;
      Pages : Page_Maps.Raw_Maps.Unsafe_Maps.Map;
      Templates : Containers.Expression_Maps.Constant_Map;
   end record;

   procedure Update
     (Builder : in out Site_Builder;
      Expression : in out S_Expressions.Lockable.Descriptor'Class);

end Natools.Web.Sites;
