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
-- Natools.Web.Sites provides a container for data about a related set of   --
-- pages (i.e. a website).                                                  --
------------------------------------------------------------------------------

with Natools.Constant_Indefinite_Ordered_Maps;
with Natools.S_Expressions.Caches;
with Natools.S_Expressions.Lockable;
with Natools.S_Expressions.Printers.Pretty;
with Natools.Web.Backends;
with Natools.Web.Containers;
with Natools.Web.Exchanges;
with Natools.Web.Filters.Stores;
with Natools.Web.Tags;

limited with Natools.Web.Sites.Updaters;
limited with Natools.Web.Sites.Updates;

private with Ada.Containers.Indefinite_Ordered_Maps;
private with Natools.S_Expressions.Atom_Refs;

package Natools.Web.Sites is

   type Site is tagged limited private;

   procedure Queue_Update
     (Object : in Site;
      Update : in Updates.Site_Update'Class);
      --  Enqueue a pending update for Object

   procedure Set_Updater
     (Object : in out Site;
      Updater : in Updaters.Updater_Access);
      --  Register an updater to handle updates for Object

   procedure Register
     (Object : in out Site;
      Name : in String;
      Constructor : in Filters.Stores.Constructor);
      --  Register a filter constructor

   procedure Reload (Object : in out Site);
      --  Reload Object data from its original file

   procedure Reset (Object : in out Site; File_Name : in String);
      --  (Re)initialize Object with data from the given file

   procedure Respond
     (Object : in out Site;
      Exchange : aliased in out Exchanges.Exchange);
      --  Look up internal data to provide a response in Exchange


   function Get_Backend (From : Site; Name : S_Expressions.Atom)
     return Backends.Backend'Class;
      --  Return a backend from its name, or raise Constraint_Error

   function Get_Filter (From : Site; Name : S_Expressions.Atom)
     return Filters.Filter'Class;
      --  Return a filter from its name, or raise Filters.Stores.No_Filter

   function Get_Tags (Object : Site) return Tags.Tag_DB;
      --  Return the whole tag database

   procedure Get_Template
     (Object : in Site;
      Name : in S_Expressions.Atom;
      Template : out S_Expressions.Caches.Cursor;
      Found : out Boolean);
      --  Retrieve a template from its name

   function Get_Template
     (Object : in Site;
      Elements : in Containers.Expression_Maps.Constant_Map;
      Expression : in out S_Expressions.Lockable.Descriptor'Class;
      Name : in S_Expressions.Atom := S_Expressions.Null_Atom;
      Lookup_Template : in Boolean := True;
      Lookup_Element : in Boolean := True;
      Lookup_Name : in Boolean := False)
     return S_Expressions.Caches.Cursor;
      --  Look for an expression named Name or Expression.Current_Atom
      --  in Elements, or Object templates, or fallback on Expression.
      --  If Name is empty and Lookup_Name is true, use the current atom in
      --  Expression as name.

   function Named_Element_Map
     (Object : in Site;
      Name : in S_Expressions.Atom)
     return Containers.Expression_Maps.Constant_Map;
      --  Return an element of the internal element map

   function Default_Template (Object : Site) return S_Expressions.Atom;
      --  Retrieve the default template name

   procedure Set_Parameters
     (Object : in Site;
      Printer : in out S_Expressions.Printers.Pretty.Printer'Class);
      --  Set pretty printer parameters to sitewide values


   type Exchange
     (Backend : not null access Exchanges.Exchange;
      Site : not null access Sites.Site)
     is limited private
     with Implicit_Dereference => Backend;


   type Page is interface;

   procedure Respond
     (Object : in out Page;
      Exchange : in out Sites.Exchange;
      Extra_Path : in S_Expressions.Atom)
     is abstract
     with Pre'Class => not Exchanges.Has_Response (Exchange.Backend.all);

   package Page_Maps is new Constant_Indefinite_Ordered_Maps
     (S_Expressions.Atom, Page'Class, S_Expressions."<");


   type Site_Builder (<>) is limited private;
      --  Temporary representation of Site objects while they are being built

   function Get_Backend (From : Site_Builder; Name : S_Expressions.Atom)
     return Backends.Backend'Class;
      --  Return a backend from its name, or raise Constraint_Error

   function Get_Filter (From : Site_Builder; Name : S_Expressions.Atom)
     return Filters.Filter'Class;
      --  Return a filter from its name, or raise Filters.Stores.No_Filter

   procedure Insert
     (Builder : in out Site_Builder;
      Path : in S_Expressions.Atom;
      New_Page : in Page'Class);
      --  Add to Builder a new page with the given web path

   procedure Insert
     (Builder : in out Site_Builder;
      Tags : in Web.Tags.Tag_List;
      Visible : in Web.Tags.Visible'Class);
      --  Add to Builder a new Visible with the given tags


   type Page_Loader is interface;

   procedure Load
     (Object : in out Page_Loader;
      Builder : in out Site_Builder;
      Path : in S_Expressions.Atom)
     is abstract;
      --  Create pages and register them in Builder


   type Page_Constructor is not null access function
     (File_Name : in S_Expressions.Atom)
     return Page_Loader'Class;
      --  Create a page loader associated with the given file name

   procedure Register
     (Self : in out Site;
      Name : in String;
      Constructor : in Page_Constructor);
      --  Regeister Constructor for Name in Self.
      --  WARNING: it is not safe to call this procedure concurrently


   type Backend_Constructor is not null access function
     (Arguments : in out S_Expressions.Lockable.Descriptor'Class)
     return Backends.Backend'Class;

   procedure Register
     (Self : in out Site;
      Name : in String;
      Constructor : in Backend_Constructor);
      --  Register Constructor for Name in Self
      --  WARNING: it is not safe to call this procedure concurrently

private

   type Exchange
     (Backend : not null access Exchanges.Exchange;
      Site : not null access Sites.Site)
     is limited null record;

   package Backend_Maps is new Constant_Indefinite_Ordered_Maps
     (S_Expressions.Atom,      Backends.Backend'Class,
      S_Expressions.Less_Than, Backends."=");

   package Backend_Constructors is new Ada.Containers.Indefinite_Ordered_Maps
     (S_Expressions.Atom, Backend_Constructor, S_Expressions.Less_Than);

   package Page_Loaders is new Constant_Indefinite_Ordered_Maps
     (S_Expressions.Atom, Page_Loader'Class, S_Expressions.Less_Than);

   package Page_Constructors is new Ada.Containers.Indefinite_Ordered_Maps
     (S_Expressions.Atom, Page_Constructor, S_Expressions.Less_Than);

   type Constructors_In_Site is record
      Backend : Backend_Constructors.Map;
      Page : Page_Constructors.Map;
   end record;

   type Site is tagged limited record
      Backends : Backend_Maps.Updatable_Map;
      Constructors : aliased Constructors_In_Site;
      Default_Template : S_Expressions.Atom_Refs.Immutable_Reference;
      File_Name : S_Expressions.Atom_Refs.Immutable_Reference;
      Filters : Web.Filters.Stores.Store;
      Loaders : Page_Loaders.Constant_Map;
      Named_Elements : Containers.Expression_Map_Maps.Constant_Map;
      Pages : Page_Maps.Updatable_Map;
      Printer_Parameters : S_Expressions.Printers.Pretty.Parameters;
      Static : Containers.Atom_Array_Refs.Immutable_Reference;
      Tags : Web.Tags.Tag_DB;
      Templates : Containers.Expression_Maps.Constant_Map;
      Updater : access Updaters.Updater'Class := null;
   end record;

   type Site_Builder
     (Constructors : not null access Constructors_In_Site)
   is limited record
      Backends : Backend_Maps.Unsafe_Maps.Map;
      Default_Template : S_Expressions.Atom_Refs.Immutable_Reference;
      File_Prefix : S_Expressions.Atom_Refs.Immutable_Reference;
      File_Suffix : S_Expressions.Atom_Refs.Immutable_Reference;
      Filters : Web.Filters.Stores.Store;
      New_Loaders : Page_Loaders.Unsafe_Maps.Map;
      Old_Loaders : Page_Loaders.Constant_Map;
      Path_Prefix : S_Expressions.Atom_Refs.Immutable_Reference;
      Path_Suffix : S_Expressions.Atom_Refs.Immutable_Reference;
      Named_Elements : Containers.Expression_Map_Maps.Constant_Map;
      Pages : Page_Maps.Unsafe_Maps.Map;
      Printer_Parameters : S_Expressions.Printers.Pretty.Parameters;
      Static : Containers.Unsafe_Atom_Lists.List;
      Tags : Web.Tags.Tag_DB_Builder;
      Templates : Containers.Expression_Maps.Constant_Map;
   end record;

   procedure Update
     (Builder : in out Site_Builder;
      Expression : in out S_Expressions.Lockable.Descriptor'Class);

end Natools.Web.Sites;
