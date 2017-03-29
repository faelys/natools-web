------------------------------------------------------------------------------
-- Copyright (c) 2015, Natacha Porté                                        --
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

package body Natools.Web.Sites.Holders is


   -----------------------------
   -- Holder Public Interface --
   -----------------------------

   overriding procedure Queue
     (Self : in out Holder;
      Update : in Updates.Site_Update'Class) is
   begin
      Self.Queue.Append (Update_Holders.To_Holder (Update));
   end Queue;


   not overriding procedure Load
     (Self : in out Holder;
      File_Name : in String)
   is
      New_Site : constant Site_Refs.Data_Access := new Site;
      New_Ref : constant Site_Refs.Reference
        := Site_Refs.Create (New_Site);
   begin
      New_Site.Constructors := Self.Constructors;
      New_Site.Filters := Self.Filters;
      New_Site.Updater := Self'Unchecked_Access;
      New_Site.Reset (File_Name);
      Self.Ref := New_Ref;
   end Load;


   not overriding procedure Register
     (Self : in out Holder;
      Name : in String;
      Constructor : in Filters.Stores.Constructor) is
   begin
      Self.Filters.Register (S_Expressions.To_Atom (Name), Constructor);
   end Register;


   not overriding procedure Register
     (Self : in out Holder;
      Name : in String;
      Constructor : in Page_Constructor) is
   begin
      Self.Constructors.Page.Insert
        (S_Expressions.To_Atom (Name), Constructor);
   end Register;


   not overriding procedure Register
     (Self : in out Holder;
      Name : in String;
      Constructor : in Backend_Constructor) is
   begin
      Self.Constructors.Backend.Insert
        (S_Expressions.To_Atom (Name), Constructor);
   end Register;


   not overriding procedure Respond
     (Self : in Holder;
      Exchange : aliased in out Exchanges.Exchange) is
   begin
      Self.Ref.Update.Respond (Exchange);
   end Respond;



   ------------------
   -- Update Queue --
   ------------------

   protected body Update_Queue is

      entry Append (Update : in Update_Holders.Holder) when True is
      begin
         if Task_Waiting then
            Task_Waiting := False;
            requeue Parent.Worker.Run;
         else
            List.Append (Update);
         end if;
      end Append;


      procedure Next (Update : out Update_Holders.Holder) is
      begin
         if List.Is_Empty then
            Task_Waiting := True;
            Update := Update_Holders.Empty_Holder;
         else
            pragma Assert (not Task_Waiting);
            Update := List.First_Element;
            List.Delete_First;
         end if;
      end Next;

   end Update_Queue;



   -------------------
   -- Update Worker --
   -------------------

   task body Worker_Task is
      Container : Update_Holders.Holder;
   begin
      loop
         Parent.Queue.Next (Container);

         if Container.Is_Empty then
            select
               accept Run (Update : in Update_Holders.Holder) do
                  Container := Update;
               end Run;
            or
               terminate;
            end select;

            exit when Container.Is_Empty;
         end if;

         declare
            Old_Site : constant Site_Refs.Accessor := Parent.Ref.Query;
            New_Site : constant Site_Refs.Data_Access := new Site'
              (Load_Date => Old_Site.Load_Date,
               ACL => Old_Site.ACL,
               Backends => Old_Site.Backends,
               Constructors => Old_Site.Constructors,
               Default_Template => Old_Site.Default_Template,
               File_Name => Old_Site.File_Name,
               Filters => Old_Site.Filters,
               Loaders => Old_Site.Loaders,
               Named_Elements => Old_Site.Named_Elements,
               Pages => Old_Site.Pages,
               Printer_Parameters => Old_Site.Printer_Parameters,
               Static => Old_Site.Static,
               Tags => Old_Site.Tags,
               Templates => Old_Site.Templates,
               Updater => Old_Site.Updater);
            New_Ref : constant Site_Refs.Reference
              := Site_Refs.Create (New_Site);
         begin
            Updates.Update (Container.Element, New_Site.all);
            Parent.Ref := New_Ref;
         end;
      end loop;
   end Worker_Task;

end Natools.Web.Sites.Holders;
