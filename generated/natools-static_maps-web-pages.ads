--  Generated at 2014-10-11 10:26:03 +0000 by Natools.Static_Hash_Maps
--  from src/natools-web-pages-maps.sx

package Natools.Static_Maps.Web.Pages is
   pragma Pure;

   type Component is
     (Error,
      Elements);

   function To_Component (Key : String) return Component;

private

   Map_1_Key_0 : aliased constant String := "elements";
   Map_1_Key_1 : aliased constant String := "layout";
   Map_1_Keys : constant array (0 .. 1) of access constant String
     := (Map_1_Key_0'Access,
         Map_1_Key_1'Access);
   Map_1_Elements : constant array (0 .. 1) of Component
     := (Elements,
         Elements);

end Natools.Static_Maps.Web.Pages;
