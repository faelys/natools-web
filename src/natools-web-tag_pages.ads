------------------------------------------------------------------------------
-- Copyright (c) 2014-2015, Natacha Porté                                   --
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
-- Natools.Web.Tag_Pages                                                    --
------------------------------------------------------------------------------

with Natools.S_Expressions.Atom_Refs;
with Natools.Web.Sites;

private with Natools.Web.Containers;

package Natools.Web.Tag_Pages is

   type Page is new Sites.Page with private;

   overriding procedure Respond
     (Object : in out Page;
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

   type Page_Mode is (Slash_Index, Slashless_Index, Child);

   type Element_Array is array (Page_Mode)
     of Containers.Expression_Maps.Constant_Map;

   type Boolean_Array is array (Page_Mode) of Boolean;

   type Page is new Sites.Page with record
      Root_Tag : S_Expressions.Atom_Refs.Immutable_Reference;
      Elements : Element_Array;
      Redirect : Boolean_Array := (others => False);
   end record;

   type Loader is new Sites.Page_Loader with record
      File : S_Expressions.Atom_Refs.Immutable_Reference;
   end record;

end Natools.Web.Tag_Pages;
