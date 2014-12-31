--  Generated at 2014-12-31 17:09:54 +0000 by Natools.Static_Hash_Maps
--  from src/natools-web-tags-maps.sx

with Natools.Static_Maps.Web.Tags.Commands;

package body Natools.Static_Maps.Web.Tags is

   function To_List_Command (Key : String) return List_Command is
      N : constant Natural
        := Natools.Static_Maps.Web.Tags.Commands.Hash (Key);
   begin
      if Map_1_Keys (N).all = Key then
         return Map_1_Elements (N);
      else
         return Unknown_List_Command;
      end if;
   end To_List_Command;

end Natools.Static_Maps.Web.Tags;
