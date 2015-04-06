------------------------------------------------------------------------------
-- Copyright (c) 2015, Natacha Porté                                        --
--                                                                          --
-- Permission to use, copy, modify, and distribute this software for any    --
-- purpose with or without fee is hereby granted, provided that the above   --
-- copyright notice and this permission notice appear in all copies.        --
--                                                                          --
-- THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES --
-- WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF         --
-- MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR  --
-- ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES   --
-- WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN    --
-- ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF  --
-- OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.           --
------------------------------------------------------------------------------

------------------------------------------------------------------------------
-- NOTE: the current implementation might be considered ugly for            --
-- duplicating the detection of path, query and fragment. The reason it is  --
-- left that way is that these part are conceptually independent, and there --
-- no constraint of having the same allowed character group. More           --
-- pragmatically that means that while they are currently identical, it     --
-- might not stay that way (e.g. restricting the allowed character set in   --
-- the fragment identifier, checking query syntax, etc.                     --
------------------------------------------------------------------------------

function Natools.Web.Is_Valid_URL (Data : String) return Boolean is
   subtype Hex is Character with Static_Predicate
     => Hex in '0' .. '9' | 'a' .. 'f' | 'A' .. 'F';

   subtype Unreserved is Character with Static_Predicate
     => Unreserved in 'a' .. 'z' | 'A' .. 'Z'             --  alpha
                    | '0' .. '9'                          --  digit
                    | '$' | '-' | '_' | '.' | '+'         --  safe
                    | '!' | '*' | ''' | '(' | ')' | ','   --  extra
                    | ';' | ':' | '@' | '&' | '=';        --  HTTP specials

   Index : Natural := Data'First;
begin
   --  Check optional HTTP or HTTPS scheme

   if Index + 6 in Data'Range
     and then Data (Index) in 'h' | 'H'
     and then Data (Index + 1) in 't' | 'T'
     and then Data (Index + 2) in 't' | 'T'
     and then Data (Index + 3) in 'p' | 'P'
   then
      if Data (Index + 4) = ':' then
         Index := Index + 5;
      elsif Data (Index + 4) in 's' | 'S' and then Data (Index + 5) = ':' then
         Index := Index + 6;
      end if;
   end if;

   --  Hierarchical part must start with an authority

   if Index + 2 not in Data'Range
     or else Data (Index) /= '/'
     or else Data (Index + 1) /= '/'
   then
      return False;
   end if;

   Index := Index + 2;

   --  Currently specifying user name and password is not support
   --  and leads to URL rejection.

   --  This is a laxist check on hostname validity

   Check_Hostname :
   while Index in Data'Range
     and then Data (Index) in 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '-' | '.'
   loop
      Index := Index + 1;
   end loop Check_Hostname;

   if Index not in Data'Range then
      return True;
   end if;

   --  Check the optinal port number

   if Data (Index) = ':'
     and then Index + 1 in Data'Range
     and then Data (Index + 1) in '0' .. '9'
   then
      Index := Index + 2;

      while Index in Data'Range and then Data (Index) in '0' .. '9' loop
         Index := Index + 1;
      end loop;

      if Index not in Data'Range then
         return True;
      end if;
   end if;

   --  Check the path part

   if Data (Index) /= '/' then
      return False;
   end if;

   Check_Path :
   loop
      Index := Index + 1;

      if Index not in Data'Range then
         return True;
      end if;

      if Data (Index) = '%' then
         if not (Index + 2 in Data'Range
           and then Data (Index + 1) in Hex
           and then Data (Index + 2) in Hex)
         then
            return False;
         end if;
      elsif Data (Index) not in Unreserved then
         exit Check_Path;
      end if;
   end loop Check_Path;

   --  Check the query part

   if Data (Index) = '?' then
      Check_Query :
      loop
         Index := Index + 1;

         if Index not in Data'Range then
            return True;
         end if;

         if Data (Index) = '%' then
            if not (Index + 2 in Data'Range
              and then Data (Index + 1) in Hex
              and then Data (Index + 2) in Hex)
            then
               return False;
            end if;
         elsif Data (Index) not in Unreserved then
            exit Check_Query;
         end if;
      end loop Check_Query;
   end if;

   --  Check the anchor part

   if Data (Index) = '#' then
      Check_Fragment :
      loop
         Index := Index + 1;

         if Index not in Data'Range then
            return True;
         end if;

         if Data (Index) = '%' then
            if not (Index + 2 in Data'Range
              and then Data (Index + 1) in Hex
              and then Data (Index + 2) in Hex)
            then
               return False;
            end if;
         elsif Data (Index) not in Unreserved then
            exit Check_Fragment;
         end if;
      end loop Check_Fragment;
   end if;

   return False;
end Natools.Web.Is_Valid_URL;
