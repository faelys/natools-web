------------------------------------------------------------------------------
-- Copyright (c) 2019, Natacha Porté                                        --
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
-- Natools.Web.Simple_Pages.Dynamic_Multipages extends the multipage idea   --
-- by allowing the generation of new pages through a POST request.          --
-- Note that there was not enough code to share with the package            --
-- Natools.Web.Simple_Pages.Multipages, so while inheriting conceptually    --
-- from it, this package is completely independent.                         --
------------------------------------------------------------------------------

with Natools.S_Expressions;
with Natools.S_Expressions.Atom_Refs;
with Natools.S_Expressions.Lockable;
with Natools.Web.Containers;
with Natools.Web.Sites;
with Natools.Web.Tags;

private with Natools.Constant_Indefinite_Ordered_Maps;
private with Natools.References;
private with Natools.S_Expressions.Caches;
private with Natools.Storage_Pools;

package Natools.Web.Simple_Pages.Dynamic_Multipages is

   type Page_Descriptor is record
      Name : S_Expressions.Atom_Refs.Immutable_Reference;
      Elements : Containers.Expression_Maps.Constant_Map;
      Tags : Web.Tags.Tag_List;
      Dates : Containers.Date_Maps.Constant_Map;
   end record;

   type Generator is interface;

   procedure Respond
     (Object : in out Generator;
      Exchange : in out Sites.Exchange;
      Extra_Path : in S_Expressions.Atom;
      Descriptor : out Page_Descriptor;
      Do_Post : out Boolean)
     is abstract;
      --  Handle the given HTTP exchange, filling in Descriptor and Do_Post
      --  to add a new page to the multipage set.

   type Generator_Constructor is not null access function
     (Arguments : in out S_Expressions.Lockable.Descriptor'Class)
     return Generator'Class;

   procedure Register
     (Name : in String;
      Constructor : in Generator_Constructor);


   type Adder is new Sites.Page with private;

   overriding procedure Respond
     (Object : in out Adder;
      Exchange : in out Sites.Exchange;
      Extra_Path : in S_Expressions.Atom);


   type Loader is new Sites.Page_Loader with private;

   overriding procedure Load
     (Object : in out Loader;
      Builder : in out Sites.Site_Builder;
      Path : in S_Expressions.Atom);

   function Create (File : in S_Expressions.Atom)
     return Sites.Page_Loader'Class;

private

   package Generator_Refs is new References
     (Generator'Class,
      Storage_Pools.Access_In_Default_Pool'Storage_Pool,
      Storage_Pools.Access_In_Default_Pool'Storage_Pool);

   type Default_Data is record
      File_Path : S_Expressions.Atom_Refs.Immutable_Reference;
      Web_Path : S_Expressions.Atom_Refs.Immutable_Reference;
      Template : Page_Template;
      Generator : Generator_Refs.Reference;
      Generator_Path : S_Expressions.Atom_Refs.Immutable_Reference;
   end record;

   package Default_Data_Refs is new References
     (Default_Data,
      Storage_Pools.Access_In_Default_Pool'Storage_Pool,
      Storage_Pools.Access_In_Default_Pool'Storage_Pool);

   type Adder is new Sites.Page with record
      Ref : Default_Data_Refs.Reference;
   end record;

   type Loader is new Sites.Page_Loader with record
      File_Path : S_Expressions.Atom_Refs.Immutable_Reference;
   end record;

   package Constructor_Maps is new Constant_Indefinite_Ordered_Maps
     (S_Expressions.Atom, Generator_Constructor, S_Expressions.Less_Than);

   Constructor_DB : Constructor_Maps.Constant_Map;

end Natools.Web.Simple_Pages.Dynamic_Multipages;
