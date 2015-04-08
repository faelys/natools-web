--  Generated at 2015-04-08 17:20:37 +0000 by Natools.Static_Hash_Maps
--  from src/natools-web-comments-maps.sx

with Natools.Static_Maps.Web.Comments.Item_Commands;
with Natools.Static_Maps.Web.Comments.Item_Elements;
with Natools.Static_Maps.Web.Comments.Item_Forms;
with Natools.Static_Maps.Web.Comments.List_Commands;
with Natools.Static_Maps.Web.Comments.List_Elements;

package body Natools.Static_Maps.Web.Comments is

   function To_Item_Command (Key : String) return Item.Command.Enum is
      N : constant Natural
        := Natools.Static_Maps.Web.Comments.Item_Commands.Hash (Key);
   begin
      if Map_1_Keys (N).all = Key then
         return Map_1_Elements (N);
      else
         return Item.Command.Unknown;
      end if;
   end To_Item_Command;


   function To_Item_Element (Key : String) return Item.Element.Enum is
      N : constant Natural
        := Natools.Static_Maps.Web.Comments.Item_Elements.Hash (Key);
   begin
      if Map_2_Keys (N).all = Key then
         return Map_2_Elements (N);
      else
         return Item.Element.Unknown;
      end if;
   end To_Item_Element;


   function To_Item_Form (Key : String) return Item.Form.Enum is
      N : constant Natural
        := Natools.Static_Maps.Web.Comments.Item_Forms.Hash (Key);
   begin
      if Map_3_Keys (N).all = Key then
         return Map_3_Elements (N);
      else
         return Item.Form.Unknown;
      end if;
   end To_Item_Form;


   function To_List_Command (Key : String) return List.Command.Enum is
      N : constant Natural
        := Natools.Static_Maps.Web.Comments.List_Commands.Hash (Key);
   begin
      if Map_4_Keys (N).all = Key then
         return Map_4_Elements (N);
      else
         return List.Command.Unknown;
      end if;
   end To_List_Command;


   function To_List_Element (Key : String) return List.Element.Enum is
      N : constant Natural
        := Natools.Static_Maps.Web.Comments.List_Elements.Hash (Key);
   begin
      if Map_5_Keys (N).all = Key then
         return Map_5_Elements (N);
      else
         return List.Element.Unknown;
      end if;
   end To_List_Element;

end Natools.Static_Maps.Web.Comments;
