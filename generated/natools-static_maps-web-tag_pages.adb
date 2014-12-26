--  Generated at 2014-12-26 20:59:31 +0000 by Natools.Static_Hash_Maps
--  from src/natools-web-tag_pages-maps.sx

with Natools.Static_Maps.Web.Tag_Pages.Commands;
with Natools.Static_Maps.Web.Tag_Pages.Components;

package body Natools.Static_Maps.Web.Tag_Pages is

   function To_Command (Key : String) return Command is
      N : constant Natural
        := Natools.Static_Maps.Web.Tag_Pages.Commands.Hash (Key);
   begin
      if Map_1_Keys (N).all = Key then
         return Map_1_Elements (N);
      else
         return Unknown_Command;
      end if;
   end To_Command;


   function To_Component (Key : String) return Component is
      N : constant Natural
        := Natools.Static_Maps.Web.Tag_Pages.Components.Hash (Key);
   begin
      if Map_2_Keys (N).all = Key then
         return Map_2_Elements (N);
      else
         return Unknown_Component;
      end if;
   end To_Component;

end Natools.Static_Maps.Web.Tag_Pages;
