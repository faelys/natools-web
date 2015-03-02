--  Generated at 2015-03-02 22:10:35 +0000 by Natools.Static_Hash_Maps
--  from src/natools-web-simple_pages-maps.sx

with Natools.Static_Maps.Web.Simple_Pages.Commands;
with Natools.Static_Maps.Web.Simple_Pages.Components;
function Natools.Static_Maps.Web.Simple_Pages.T
  return Boolean is
begin
   for I in Map_1_Keys'Range loop
      if Natools.Static_Maps.Web.Simple_Pages.Commands.Hash
           (Map_1_Keys (I).all) /= I
      then
         return False;
      end if;
   end loop;

   for I in Map_2_Keys'Range loop
      if Natools.Static_Maps.Web.Simple_Pages.Components.Hash
           (Map_2_Keys (I).all) /= I
      then
         return False;
      end if;
   end loop;

   return True;
end Natools.Static_Maps.Web.Simple_Pages.T;
