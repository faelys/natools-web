--  Generated at 2017-03-27 17:51:44 +0000 by Natools.Static_Hash_Maps
--  from src/natools-web-acl-maps.sx

with Natools.Static_Maps.Web.ACL.Commands;

package body Natools.Static_Maps.Web.ACL is

   function To_Command (Key : String) return Command is
      N : constant Natural
        := Natools.Static_Maps.Web.ACL.Commands.Hash (Key);
   begin
      if Map_1_Keys (N).all = Key then
         return Map_1_Elements (N);
      else
         return Unknown_Command;
      end if;
   end To_Command;

end Natools.Static_Maps.Web.ACL;
