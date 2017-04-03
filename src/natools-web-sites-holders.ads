------------------------------------------------------------------------------
-- Copyright (c) 2015-2017, Natacha Porté                                   --
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
-- Natools.Web.Sites.Holders provides an updatable site holder, that as     --
-- task-safe as Natools.References.                                         --
------------------------------------------------------------------------------

with Natools.Web.Sites.Updaters;
with Natools.Web.Sites.Updates;

private with Ada.Containers.Doubly_Linked_Lists;
private with Ada.Containers.Indefinite_Holders;
private with Natools.References;
private with Natools.Storage_Pools;

package Natools.Web.Sites.Holders is

   type Holder is limited new Updaters.Updater with private;

   overriding procedure Queue
     (Self : in out Holder;
      Update : in Updates.Site_Update'Class);

   not overriding procedure Load
     (Self : in out Holder;
      File_Name : in String);

   not overriding procedure Register
     (Self : in out Holder;
      Name : in String;
      Constructor : in Filters.Stores.Constructor);

   not overriding procedure Register
     (Self : in out Holder;
      Name : in String;
      Constructor : in Page_Constructor);

   not overriding procedure Register
     (Self : in out Holder;
      Name : in String;
      Constructor : in Backend_Constructor);

   not overriding procedure Register
     (Self : in out Holder;
      Name : in String;
      Constructor : in ACL_Constructor);

   not overriding procedure Respond
     (Self : in Holder;
      Exchange : aliased in out Exchanges.Exchange);

private

   package Site_Refs is new References
     (Site,
      Storage_Pools.Access_In_Default_Pool'Storage_Pool,
      Storage_Pools.Access_In_Default_Pool'Storage_Pool);

   package Update_Holders is new Ada.Containers.Indefinite_Holders
     (Updates.Site_Update'Class, Updates."=");

   package Update_Lists is new Ada.Containers.Doubly_Linked_Lists
     (Update_Holders.Holder, Update_Holders."=");


   protected type Update_Queue (Parent : not null access Holder) is
      entry Append (Update : in Update_Holders.Holder);
      procedure Next (Update : out Update_Holders.Holder);
   private
      Task_Waiting : Boolean := False;
      List : Update_Lists.List;
   end Update_Queue;

   task type Worker_Task (Parent : not null access Holder) is
      entry Run (Update : in Update_Holders.Holder);
   end Worker_Task;


   type Holder is limited new Updaters.Updater with record
      Ref : Site_Refs.Reference;
      Constructors : Constructors_In_Site;
      Filters : Web.Filters.Stores.Store;
      Worker : Worker_Task (Holder'Access);
      Queue : Update_Queue (Holder'Access);
   end record;

end Natools.Web.Sites.Holders;
