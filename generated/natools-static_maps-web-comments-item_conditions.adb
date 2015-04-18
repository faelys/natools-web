with Interfaces; use Interfaces;

package body Natools.Static_Maps.Web.Comments.Item_Conditions is

   P : constant array (0 .. 3) of Natural :=
     (1, 5, 16, 18);

   T1 : constant array (0 .. 3) of Unsigned_8 :=
     (20, 4, 8, 3);

   T2 : constant array (0 .. 3) of Unsigned_8 :=
     (4, 22, 3, 16);

   G : constant array (0 .. 24) of Unsigned_8 :=
     (5, 8, 0, 0, 0, 0, 0, 3, 1, 0, 4, 5, 7, 0, 7, 11, 0, 0, 0, 0, 4, 0, 0,
      0, 0);

   function Hash (S : String) return Natural is
      F : constant Natural := S'First - 1;
      L : constant Natural := S'Length;
      F1, F2 : Natural := 0;
      J : Natural;
   begin
      for K in P'Range loop
         exit when L < P (K);
         J  := Character'Pos (S (P (K) + F));
         F1 := (F1 + Natural (T1 (K)) * J) mod 25;
         F2 := (F2 + Natural (T2 (K)) * J) mod 25;
      end loop;
      return (Natural (G (F1)) + Natural (G (F2))) mod 12;
   end Hash;

end Natools.Static_Maps.Web.Comments.Item_Conditions;
