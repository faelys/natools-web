--  Generated at 2015-04-08 17:20:37 +0000 by Natools.Static_Hash_Maps
--  from src/natools-web-comments-maps.sx

package Natools.Static_Maps.Web.Comments is
   pragma Pure;

   package Item is
      package Command is
         type Enum is (Unknown, Date, Id, Name, Mail, Link, Text, Parent,
           If_Name, If_No_Name, If_Mail, If_No_Mail, If_Link, If_No_Link);
      end Command;

      package Element is
         type Enum is (Unknown, Date, Name, Mail, Link, Text);
      end Element;

      package Form is
         type Enum is (Unknown, Name, Mail, Link, Text);
      end Form;
   end Item;

   package List is
      package Command is
         type Enum is (Unknown, List, Preview, Size);
      end Command;

      package Element is
         type Enum is (Unknown, Backend, Tags);
      end Element;
   end List;

   function To_Item_Command (Key : String) return Item.Command.Enum;
   function To_Item_Element (Key : String) return Item.Element.Enum;
   function To_Item_Form (Key : String) return Item.Form.Enum;
   function To_List_Command (Key : String) return List.Command.Enum;
   function To_List_Element (Key : String) return List.Element.Enum;

private

   Map_1_Key_0 : aliased constant String := "date";
   Map_1_Key_1 : aliased constant String := "id";
   Map_1_Key_2 : aliased constant String := "if-link";
   Map_1_Key_3 : aliased constant String := "if-no-link";
   Map_1_Key_4 : aliased constant String := "if-no-mail";
   Map_1_Key_5 : aliased constant String := "if-no-name";
   Map_1_Key_6 : aliased constant String := "if-mail";
   Map_1_Key_7 : aliased constant String := "if-name";
   Map_1_Key_8 : aliased constant String := "name";
   Map_1_Key_9 : aliased constant String := "mail";
   Map_1_Key_10 : aliased constant String := "e-mail";
   Map_1_Key_11 : aliased constant String := "link";
   Map_1_Key_12 : aliased constant String := "www";
   Map_1_Key_13 : aliased constant String := "site";
   Map_1_Key_14 : aliased constant String := "website";
   Map_1_Key_15 : aliased constant String := "parent";
   Map_1_Key_16 : aliased constant String := "text";
   Map_1_Keys : constant array (0 .. 16) of access constant String
     := (Map_1_Key_0'Access,
         Map_1_Key_1'Access,
         Map_1_Key_2'Access,
         Map_1_Key_3'Access,
         Map_1_Key_4'Access,
         Map_1_Key_5'Access,
         Map_1_Key_6'Access,
         Map_1_Key_7'Access,
         Map_1_Key_8'Access,
         Map_1_Key_9'Access,
         Map_1_Key_10'Access,
         Map_1_Key_11'Access,
         Map_1_Key_12'Access,
         Map_1_Key_13'Access,
         Map_1_Key_14'Access,
         Map_1_Key_15'Access,
         Map_1_Key_16'Access);
   Map_1_Elements : constant array (0 .. 16) of Item.Command.Enum
     := (Item.Command.Date,
         Item.Command.Id,
         Item.Command.If_Link,
         Item.Command.If_No_Link,
         Item.Command.If_No_Mail,
         Item.Command.If_No_Name,
         Item.Command.If_Mail,
         Item.Command.If_Name,
         Item.Command.Name,
         Item.Command.Mail,
         Item.Command.Mail,
         Item.Command.Link,
         Item.Command.Link,
         Item.Command.Link,
         Item.Command.Link,
         Item.Command.Parent,
         Item.Command.Text);

   Map_2_Key_0 : aliased constant String := "date";
   Map_2_Key_1 : aliased constant String := "name";
   Map_2_Key_2 : aliased constant String := "mail";
   Map_2_Key_3 : aliased constant String := "e-mail";
   Map_2_Key_4 : aliased constant String := "link";
   Map_2_Key_5 : aliased constant String := "www";
   Map_2_Key_6 : aliased constant String := "site";
   Map_2_Key_7 : aliased constant String := "website";
   Map_2_Key_8 : aliased constant String := "text";
   Map_2_Keys : constant array (0 .. 8) of access constant String
     := (Map_2_Key_0'Access,
         Map_2_Key_1'Access,
         Map_2_Key_2'Access,
         Map_2_Key_3'Access,
         Map_2_Key_4'Access,
         Map_2_Key_5'Access,
         Map_2_Key_6'Access,
         Map_2_Key_7'Access,
         Map_2_Key_8'Access);
   Map_2_Elements : constant array (0 .. 8) of Item.Element.Enum
     := (Item.Element.Date,
         Item.Element.Name,
         Item.Element.Mail,
         Item.Element.Mail,
         Item.Element.Link,
         Item.Element.Link,
         Item.Element.Link,
         Item.Element.Link,
         Item.Element.Text);

   Map_3_Key_0 : aliased constant String := "c_name";
   Map_3_Key_1 : aliased constant String := "c_mail";
   Map_3_Key_2 : aliased constant String := "c_site";
   Map_3_Key_3 : aliased constant String := "c_text";
   Map_3_Keys : constant array (0 .. 3) of access constant String
     := (Map_3_Key_0'Access,
         Map_3_Key_1'Access,
         Map_3_Key_2'Access,
         Map_3_Key_3'Access);
   Map_3_Elements : constant array (0 .. 3) of Item.Form.Enum
     := (Item.Form.Name,
         Item.Form.Mail,
         Item.Form.Link,
         Item.Form.Text);

   Map_4_Key_0 : aliased constant String := "list";
   Map_4_Key_1 : aliased constant String := "preview";
   Map_4_Key_2 : aliased constant String := "size";
   Map_4_Key_3 : aliased constant String := "length";
   Map_4_Keys : constant array (0 .. 3) of access constant String
     := (Map_4_Key_0'Access,
         Map_4_Key_1'Access,
         Map_4_Key_2'Access,
         Map_4_Key_3'Access);
   Map_4_Elements : constant array (0 .. 3) of List.Command.Enum
     := (List.Command.List,
         List.Command.Preview,
         List.Command.Size,
         List.Command.Size);

   Map_5_Key_0 : aliased constant String := "backend";
   Map_5_Key_1 : aliased constant String := "tags";
   Map_5_Keys : constant array (0 .. 1) of access constant String
     := (Map_5_Key_0'Access,
         Map_5_Key_1'Access);
   Map_5_Elements : constant array (0 .. 1) of List.Element.Enum
     := (List.Element.Backend,
         List.Element.Tags);

end Natools.Static_Maps.Web.Comments;
