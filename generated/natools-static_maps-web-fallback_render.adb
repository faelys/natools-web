--  Generated at 2017-04-01 17:51:32 +0000 by Natools.Static_Hash_Maps
--  from src/natools-web-fallback_render-maps.sx

with Natools.Static_Maps.Web.Fallback_Render.Commands;

package body Natools.Static_Maps.Web.Fallback_Render is

   function To_Command (Key : String) return Command is
      N : constant Natural
        := Natools.Static_Maps.Web.Fallback_Render.Commands.Hash (Key);
   begin
      if Map_1_Keys (N).all = Key then
         return Map_1_Elements (N);
      else
         return Unknown;
      end if;
   end To_Command;

end Natools.Static_Maps.Web.Fallback_Render;
