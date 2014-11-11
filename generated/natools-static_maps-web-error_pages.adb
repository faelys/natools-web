--  Generated at 2014-11-11 19:00:28 +0000 by Natools.Static_Hash_Maps
--  from src/natools-web-error_pages-maps.sx

with Natools.Static_Maps.Web.Error_Pages.Commands;
with Natools.Static_Maps.Web.Error_Pages.Messages;

package body Natools.Static_Maps.Web.Error_Pages is

   function To_Command (Key : String) return Command is
      N : constant Natural
        := Natools.Static_Maps.Web.Error_Pages.Commands.Hash (Key);
   begin
      if Map_1_Keys (N).all = Key then
         return Map_1_Elements (N);
      else
         return Unknown_Command;
      end if;
   end To_Command;


   function Map_2_Elements (Hash : Map_2_Hash)
     return String is
   begin
      case Hash is
         when 0 =>
            return "Not Found";
         when 1 =>
            return "Gone";
      end case;
   end Map_2_Elements;

   function To_Message (Key : String) return String is
      N : constant Natural
        := Natools.Static_Maps.Web.Error_Pages.Messages.Hash (Key);
   begin
      if Map_2_Keys (N).all = Key then
         return Map_2_Elements (N);
      else
         raise Constraint_Error with "Key """ & Key & """ not in map";
      end if;
   end To_Message;

end Natools.Static_Maps.Web.Error_Pages;
