--  Generated at 2014-10-14 19:58:04 +0000 by Natools.Static_Hash_Maps
--  from src/natools-web-sites-maps.sx

with Natools.Static_Maps.Web.Sites.Commands;

package body Natools.Static_Maps.Web.Sites is

   function To_Command (Key : String) return Command is
      N : constant Natural
        := Natools.Static_Maps.Web.Sites.Commands.Hash (Key);
   begin
      if Map_1_Keys (N).all = Key then
         return Map_1_Elements (N);
      else
         return Error;
      end if;
   end To_Command;

end Natools.Static_Maps.Web.Sites;
