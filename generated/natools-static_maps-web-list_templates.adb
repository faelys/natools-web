--  Generated at 2015-07-28 19:04:59 +0000 by Natools.Static_Hash_Maps
--  from src/natools-web-list_templates-maps.sx

with Natools.Static_Maps.Web.List_Templates.Commands;

package body Natools.Static_Maps.Web.List_Templates is

   function To_Command (Key : String) return Command is
      N : constant Natural
        := Natools.Static_Maps.Web.List_Templates.Commands.Hash (Key);
   begin
      if Map_1_Keys (N).all = Key then
         return Map_1_Elements (N);
      else
         return Unknown_Command;
      end if;
   end To_Command;

end Natools.Static_Maps.Web.List_Templates;
