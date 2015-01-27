--  Generated at 2015-01-27 19:24:23 +0000 by Natools.Static_Hash_Maps
--  from src/natools-web-comments-maps.sx

with Natools.Static_Maps.Web.Comments.Item_Commands;
with Natools.Static_Maps.Web.Comments.Item_Elements;
with Natools.Static_Maps.Web.Comments.List_Commands;
with Natools.Static_Maps.Web.Comments.List_Elements;
function Natools.Static_Maps.Web.Comments.T
  return Boolean is
begin
   for I in Map_1_Keys'Range loop
      if Natools.Static_Maps.Web.Comments.Item_Commands.Hash
           (Map_1_Keys (I).all) /= I
      then
         return False;
      end if;
   end loop;

   for I in Map_2_Keys'Range loop
      if Natools.Static_Maps.Web.Comments.Item_Elements.Hash
           (Map_2_Keys (I).all) /= I
      then
         return False;
      end if;
   end loop;

   for I in Map_3_Keys'Range loop
      if Natools.Static_Maps.Web.Comments.List_Commands.Hash
           (Map_3_Keys (I).all) /= I
      then
         return False;
      end if;
   end loop;

   for I in Map_4_Keys'Range loop
      if Natools.Static_Maps.Web.Comments.List_Elements.Hash
           (Map_4_Keys (I).all) /= I
      then
         return False;
      end if;
   end loop;

   return True;
end Natools.Static_Maps.Web.Comments.T;
