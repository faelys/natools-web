--  Generated at 2015-02-03 21:39:53 +0000 by Natools.Static_Hash_Maps
--  from src/natools-web-error_pages-maps.sx

with Natools.Static_Maps.Web.Error_Pages.Commands;
with Natools.Static_Maps.Web.Error_Pages.Messages;
function Natools.Static_Maps.Web.Error_Pages.T
  return Boolean is
begin
   for I in Map_1_Keys'Range loop
      if Natools.Static_Maps.Web.Error_Pages.Commands.Hash
           (Map_1_Keys (I).all) /= I
      then
         return False;
      end if;
   end loop;

   for I in Map_2_Keys'Range loop
      if Natools.Static_Maps.Web.Error_Pages.Messages.Hash
           (Map_2_Keys (I).all) /= I
      then
         return False;
      end if;
   end loop;

   return True;
end Natools.Static_Maps.Web.Error_Pages.T;
