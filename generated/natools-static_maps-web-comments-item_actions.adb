with Interfaces; use Interfaces;

package body Natools.Static_Maps.Web.Comments.Item_Actions is

   P : constant array (0 .. 0) of Natural :=
     (0 .. 0 => 3);

   T1 : constant array (0 .. 0) of Unsigned_8 :=
     (0 .. 0 => 12);

   T2 : constant array (0 .. 0) of Unsigned_8 :=
     (0 .. 0 => 23);

   G : constant array (0 .. 29) of Unsigned_8 :=
     (0, 0, 0, 0, 0, 13, 0, 0, 5, 0, 7, 8, 4, 1, 11, 4, 0, 0, 2, 0, 1, 0, 0,
      0, 0, 0, 0, 8, 8, 3);

   function Hash (S : String) return Natural is
      F : constant Natural := S'First - 1;
      L : constant Natural := S'Length;
      F1, F2 : Natural := 0;
      J : Natural;
   begin
      for K in P'Range loop
         exit when L < P (K);
         J  := Character'Pos (S (P (K) + F));
         F1 := (F1 + Natural (T1 (K)) * J) mod 30;
         F2 := (F2 + Natural (T2 (K)) * J) mod 30;
      end loop;
      return (Natural (G (F1)) + Natural (G (F2))) mod 14;
   end Hash;

end Natools.Static_Maps.Web.Comments.Item_Actions;
