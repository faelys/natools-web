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
-- Common package holds a single Natools.Web.Sites.Site object and          --
-- provides an AWS callback to respond with it.                             --
------------------------------------------------------------------------------

with AWS.Response;
with AWS.Status;
with Natools.Web.Sites.Holders;
with Natools.Web.Sites.Updates;

package Common is

   type Holder is new Natools.Web.Sites.Holders.Holder with null record;

   overriding procedure Queue
     (Object : in out Holder;
      Update : in Natools.Web.Sites.Updates.Site_Update'Class);

   overriding procedure Load
     (Self : in out Holder;
      File_Name : in String);


   Site : Holder;

   function Respond (Request : AWS.Status.Data) return AWS.Response.Data;

   procedure Text_IO_Log
     (Severity : in Natools.Web.Severities.Code;
      Message : in String);

   procedure Syslog_Log
     (Severity : in Natools.Web.Severities.Code;
      Message : in String);

private

   protected Counter is
      function Get_Value return Natural;
      entry Wait_Version (Minimum : in Natural);
      entry Increment;
   private
      entry Block_Until_Increment (Minimum : in Natural);
      entry Release_Block;
      Value : Natural := 0;
   end Counter;


   type Increment_Count is new  Natools.Web.Sites.Updates.Site_Update
     with null record;

   overriding procedure Update
     (Self : in Increment_Count;
      Object : in out Natools.Web.Sites.Site);

end Common;
