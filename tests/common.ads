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
-- Common package holds a single Natools.Web.Sites.Site object and          --
-- provides an AWS callback to respond with it.                             --
------------------------------------------------------------------------------

with AWS.Response;
with AWS.Status;
with Natools.Web.Sites;

package Common is

   Site : Natools.Web.Sites.Site;

   function Respond (Request : AWS.Status.Data) return AWS.Response.Data;

   procedure Text_IO_Log
     (Severity : in Natools.Web.Severities.Code;
      Message : in String);

   procedure Syslog_Log
     (Severity : in Natools.Web.Severities.Code;
      Message : in String);

end Common;
