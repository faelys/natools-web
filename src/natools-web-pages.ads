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

with Natools.S_Expressions;

private with Natools.References;
private with Natools.S_Expressions.Atom_Refs;
private with Natools.Storage_Pools;
private with Natools.Web.Containers;

package Natools.Web.Pages is

   type Page_Ref is tagged private;

   function Create (File_Path, Web_Path : in S_Expressions.Atom)
     return Page_Ref;

private

   type Page_Data is tagged limited record
      File_Path : S_Expressions.Atom_Refs.Immutable_Reference;
      Web_Path : S_Expressions.Atom_Refs.Immutable_Reference;
      Elements : Containers.Expression_Maps.Constant_Map;
   end record;

   package Data_Refs is new References
     (Page_Data,
      Storage_Pools.Access_In_Default_Pool'Storage_Pool,
      Storage_Pools.Access_In_Default_Pool'Storage_Pool);

   type Page_Ref is tagged record
      Ref : Data_Refs.Reference;
   end record;

end Natools.Web.Pages;
