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
-- IMPLEMENTATION NOTES                                                     --
-- Counter is a "simple" protected holding a natural value that can be      --
-- incremented, and for which a given threshold can be waited.              --
-- It should be free of race conditions.                                    --
-- The general architecture of Counter can be explained by considering two  --
-- states: a long-lived "base" state, when `Release_Block` queue is empty,  --
-- and a short-lived "pulsing" state, when `Release_Block` holds a task in  --
-- its queue.                                                               --
-- In base state, waiting tasks call `Wait_Version`, which either exists    --
-- immediately or requeues them in `Block_Until_Increment` where they stay  --
-- blocked until entering the pulsing state.                                --
-- In pulsing state, public entries are blocked, and all tasks in           --
-- `Block_Until_Increment` queue are requeued in `Wait_Version`. When this  --
-- is done, pulsing state ends.                                             --
-- Immediately after coming back to base state, waiting tasks execute       --
-- `Wait_Version`, which unlocks them or requeues them again in             --
-- `Block_Until_Increment`.                                                 --
-- Note that right after coming back to base state, there might be both     --
-- waiting tasks queued on `Wait_Version` and updating tasks queued on      --
-- `Increment`. The order in which they are serviced should be semantically --
-- equivalent, but servicing `Increment` first lowers the number of         --
-- requeues and should therefore be a bit more efficient.                   --
------------------------------------------------------------------------------

with Ada.Calendar.Formatting;
with Ada.Text_IO;
with Natools.Web.Exchanges;
with Natools.Web.Sites.Test_Updates;
with Syslog;

package body Common is

   protected body Counter is

      function Get_Value return Natural is
      begin
         return Value;
      end Get_Value;

      entry Wait_Version (Minimum : in Natural)
        when Release_Block'Count = 0 and then Increment'Count = 0 is
      begin
         if Minimum > Value then
            requeue Block_Until_Increment;
         end if;
      end Wait_Version;

      entry Increment when Release_Block'Count = 0 is
      begin
         Value := Value + 1;
         requeue Release_Block;
      end Increment;

      entry Block_Until_Increment (Minimum : in Natural)
        when Release_Block'Count > 0 is
      begin
         requeue Wait_Version;
      end Block_Until_Increment;

      entry Release_Block when Block_Until_Increment'Count = 0 is
      begin
         null;
      end Release_Block;

   end Counter;



   overriding procedure Queue
     (Object : in out Holder;
      Update : in Natools.Web.Sites.Updates.Site_Update'Class)
   is
      package Holders renames Natools.Web.Sites.Holders;
   begin
      Holders.Queue (Holders.Holder (Object), Update);
      Holders.Queue (Holders.Holder (Object), Increment_Count'(null record));
   end Queue;


   overriding procedure Load
     (Self : in out Holder;
      File_Name : in String)
   is
      package Holders renames Natools.Web.Sites.Holders;
   begin
      Holders.Load (Holders.Holder (Self), File_Name);
      Holders.Queue
        (Holders.Holder (Self),
         Natools.Web.Sites.Test_Updates.Load_Date_Override'
           (New_Date => Ada.Calendar.Formatting.Time_Of
              (1980, 1, 1,   10, 30, 42)));
   end Load;


   not overriding procedure Purge (Self : in out Holder) is
      package Holders renames Natools.Web.Sites.Holders;
      Update : Natools.Web.Sites.Updates.Expiration_Purger;
   begin
      Holders.Queue (Holders.Holder (Self), Update);
   end Purge;



   overriding procedure Update
     (Self : in Increment_Count;
      Object : in out Natools.Web.Sites.Site)
   is
      pragma Unreferenced (Self, Object);
   begin
      Counter.Increment;
   end Update;



   function Respond (Request : AWS.Status.Data) return AWS.Response.Data is
      Aliased_Request : aliased constant AWS.Status.Data := Request;
      Exchange : aliased Natools.Web.Exchanges.Exchange
        (Aliased_Request'Access);
   begin
      Site.Respond (Exchange);
      return Natools.Web.Exchanges.Response (Exchange);
   end Respond;


   procedure Text_IO_Log
     (Severity : in Natools.Web.Severities.Code;
      Message : in String) is
   begin
      Ada.Text_IO.Put_Line
        ('[' & Natools.Web.Severities.Code'Image (Severity) & "] "
         & Message);
   end Text_IO_Log;


   Severity_Table : constant array (Natools.Web.Severities.Code)
     of Syslog.Severities.Code
     := (Natools.Web.Severities.Critical => Syslog.Severities.Critical,
         Natools.Web.Severities.Error    => Syslog.Severities.Error,
         Natools.Web.Severities.Warning  => Syslog.Severities.Warning,
         Natools.Web.Severities.Info     => Syslog.Severities.Informational);

   procedure Syslog_Log
     (Severity : in Natools.Web.Severities.Code;
      Message : in String) is
   begin
      Syslog.Log (Severity_Table (Severity), Message);
   end Syslog_Log;

end Common;
