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

with Ada.Command_Line;
with Ada.Directories;
with Ada.Text_IO;
with AWS.Config;
with AWS.Server;
with Common;
with Natools.Web.Sites;
with Syslog.Guess.App_Name;
with Syslog.Guess.Hostname;
with Syslog.Transport.Send_Task;
with Syslog.Transport.UDP;

procedure Simple_Site is
   WS : AWS.Server.HTTP;
   Debug : constant Boolean := Ada.Command_Line.Argument_Count >= 2;
begin
   if Debug then
      Natools.Web.Log := Common.Text_IO_Log'Access;
   else
      Syslog.Guess.App_Name;
      Syslog.Guess.Hostname;
      Syslog.Transport.UDP.Connect ("127.0.0.1");
      Syslog.Transport.Send_Task.Set_Backend (Syslog.Transport.UDP.Transport);
      Syslog.Set_Transport (Syslog.Transport.Send_Task.Transport);
      Syslog.Set_Default_Facility (Syslog.Facilities.Daemon);
      Natools.Web.Log := Common.Syslog_Log'Access;
   end if;

   if Ada.Command_Line.Argument_Count >= 1 then
      Natools.Web.Sites.Reset (Common.Site, Ada.Command_Line.Argument (1));
   else
      Natools.Web.Sites.Reset (Common.Site, "site.sx");
   end if;

   AWS.Server.Start (WS, Common.Respond'Access, AWS.Config.Get_Current);

   if not Debug then
      AWS.Server.Wait;
   elsif Ada.Directories.Exists (Ada.Command_Line.Argument (2)) then
      Ada.Text_IO.Put_Line ("Websever started, waiting for removal of "
        & Ada.Command_Line.Argument (2));
      loop
         delay 1.0;
         exit when not Ada.Directories.Exists (Ada.Command_Line.Argument (2));
      end loop;
   else
      Ada.Text_IO.Put_Line ("Websever started, waiting for Q press");
      AWS.Server.Wait (AWS.Server.Q_Key_Pressed);
   end if;

   AWS.Server.Shutdown (WS);
end Simple_Site;
