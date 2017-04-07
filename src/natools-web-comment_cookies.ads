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
-- Natools.Web.Comment_Cookies provides an object to store persitent        --
-- information for comment forms.                                           --
------------------------------------------------------------------------------

with Natools.S_Expressions.Atom_Refs;
with Natools.S_Expressions.Lockable;

package Natools.Web.Comment_Cookies is
   pragma Preelaborate;

   type Comment_Info is private;

   Null_Info : constant Comment_Info;

   function Create
     (Name : in S_Expressions.Atom_Refs.Immutable_Reference;
      Mail : in S_Expressions.Atom_Refs.Immutable_Reference;
      Link : in S_Expressions.Atom_Refs.Immutable_Reference;
      Filter : in S_Expressions.Atom_Refs.Immutable_Reference)
     return Comment_Info;

   function Create
     (Expression : in out S_Expressions.Lockable.Descriptor'Class)
     return Comment_Info;


   function Name (Info : in Comment_Info)
     return S_Expressions.Atom_Refs.Immutable_Reference;

   function Mail (Info : in Comment_Info)
     return S_Expressions.Atom_Refs.Immutable_Reference;

   function Link (Info : in Comment_Info)
     return S_Expressions.Atom_Refs.Immutable_Reference;

   function Filter (Info : in Comment_Info)
     return S_Expressions.Atom_Refs.Immutable_Reference;

private

   type Atom_Kind is (Name, Mail, Link, Filter);

   type Ref_Array is array (Atom_Kind)
     of S_Expressions.Atom_Refs.Immutable_Reference;

   type Comment_Info is record
      Refs : Ref_Array;
   end record;


   function Name (Info : in Comment_Info)
     return S_Expressions.Atom_Refs.Immutable_Reference
     is (Info.Refs (Name));

   function Mail (Info : in Comment_Info)
     return S_Expressions.Atom_Refs.Immutable_Reference
     is (Info.Refs (Mail));

   function Link (Info : in Comment_Info)
     return S_Expressions.Atom_Refs.Immutable_Reference
     is (Info.Refs (Link));

   function Filter (Info : in Comment_Info)
     return S_Expressions.Atom_Refs.Immutable_Reference
     is (Info.Refs (Filter));

   Null_Info : constant Comment_Info
     := (Refs => (others => S_Expressions.Atom_Refs.Null_Immutable_Reference));

end Natools.Web.Comment_Cookies;
