with Interfaces; use Interfaces;

package body Natools.Static_Maps.Web.Comments.Item_Conditions is

   P : constant array (0 .. 3) of Natural :=
     (1, 12, 16, 18);

   T1 : constant array (0 .. 3) of Unsigned_8 :=
     (10, 14, 13, 15);

   T2 : constant array (0 .. 3) of Unsigned_8 :=
     (12, 23, 23, 15);

   G : constant array (0 .. 29) of Unsigned_8 :=
     (5, 0, 0, 1, 0, 0, 7, 3, 0, 11, 0, 0, 8, 0, 4, 7, 0, 0, 11, 0, 5, 0, 0,
      0, 0, 0, 1, 4, 0, 1);

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

end Natools.Static_Maps.Web.Comments.Item_Conditions;
