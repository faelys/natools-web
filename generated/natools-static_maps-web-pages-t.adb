--  Generated at 2014-10-11 10:26:03 +0000 by Natools.Static_Hash_Maps
--  from src/natools-web-pages-maps.sx

with Natools.Static_Maps.Web.Pages.Components;
function Natools.Static_Maps.Web.Pages.T
  return Boolean is
begin
   for I in Map_1_Keys'Range loop
      if Natools.Static_Maps.Web.Pages.Components.Hash
           (Map_1_Keys (I).all) /= I
      then
         return False;
      end if;
   end loop;

   return True;
end Natools.Static_Maps.Web.Pages.T;
