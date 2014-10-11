--  Generated at 2014-10-11 10:26:03 +0000 by Natools.Static_Hash_Maps
--  from src/natools-web-pages-maps.sx

with Natools.Static_Maps.Web.Pages.Components;

package body Natools.Static_Maps.Web.Pages is

   function To_Component (Key : String) return Component is
      N : constant Natural
        := Natools.Static_Maps.Web.Pages.Components.Hash (Key);
   begin
      if Map_1_Keys (N).all = Key then
         return Map_1_Elements (N);
      else
         return Error;
      end if;
   end To_Component;

end Natools.Static_Maps.Web.Pages;
