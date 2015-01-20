--  Generated at 2015-01-20 22:28:55 +0000 by Natools.Static_Hash_Maps
--  from src/natools-web-comments-maps.sx

package Natools.Static_Maps.Web.Comments is
   pragma Pure;

   package Item is
      package Command is
         type Enum is (Unknown, Date, Name, Mail, Link, Text);
      end Command;

--    package Element is
--       type Enum is (Unknown, Date, Name, Mail, Link, Text);
--    end Element;
   end Item;

   package List is
      package Command is
         type Enum is (Unknown, List, Size);
      end Command;

      package Element is
         type Enum is (Unknown, Backend, Tags);
      end Element;
   end List;

   function To_Item_Command (Key : String) return Item.Command.Enum;
   function To_List_Command (Key : String) return List.Command.Enum;
   function To_List_Element (Key : String) return List.Element.Enum;

private

   Map_1_Key_0 : aliased constant String := "date";
   Map_1_Key_1 : aliased constant String := "name";
   Map_1_Key_2 : aliased constant String := "mail";
   Map_1_Key_3 : aliased constant String := "e-mail";
   Map_1_Key_4 : aliased constant String := "link";
   Map_1_Key_5 : aliased constant String := "www";
   Map_1_Key_6 : aliased constant String := "site";
   Map_1_Key_7 : aliased constant String := "website";
   Map_1_Key_8 : aliased constant String := "text";
   Map_1_Keys : constant array (0 .. 8) of access constant String
     := (Map_1_Key_0'Access,
         Map_1_Key_1'Access,
         Map_1_Key_2'Access,
         Map_1_Key_3'Access,
         Map_1_Key_4'Access,
         Map_1_Key_5'Access,
         Map_1_Key_6'Access,
         Map_1_Key_7'Access,
         Map_1_Key_8'Access);
   Map_1_Elements : constant array (0 .. 8) of Item.Command.Enum
     := (Item.Command.Date,
         Item.Command.Name,
         Item.Command.Mail,
         Item.Command.Mail,
         Item.Command.Link,
         Item.Command.Link,
         Item.Command.Link,
         Item.Command.Link,
         Item.Command.Text);

   Map_2_Key_0 : aliased constant String := "list";
   Map_2_Key_1 : aliased constant String := "size";
   Map_2_Key_2 : aliased constant String := "length";
   Map_2_Keys : constant array (0 .. 2) of access constant String
     := (Map_2_Key_0'Access,
         Map_2_Key_1'Access,
         Map_2_Key_2'Access);
   Map_2_Elements : constant array (0 .. 2) of List.Command.Enum
     := (List.Command.List,
         List.Command.Size,
         List.Command.Size);

   Map_3_Key_0 : aliased constant String := "backend";
   Map_3_Key_1 : aliased constant String := "Tags";
   Map_3_Keys : constant array (0 .. 1) of access constant String
     := (Map_3_Key_0'Access,
         Map_3_Key_1'Access);
   Map_3_Elements : constant array (0 .. 1) of List.Element.Enum
     := (List.Element.Backend,
         List.Element.Tags);

end Natools.Static_Maps.Web.Comments;