with Interfaces; use Interfaces;

package body Natools.Static_Maps.Web.Comments.Item_Conditions is

   P : constant array (0 .. 3) of Natural :=
     (1, 2, 16, 18);

   T1 : constant array (0 .. 3) of Unsigned_8 :=
     (22, 13, 21, 14);

   T2 : constant array (0 .. 3) of Unsigned_8 :=
     (2, 26, 4, 3);

   G : constant array (0 .. 28) of Unsigned_8 :=
     (0, 9, 11, 0, 0, 0, 0, 0, 12, 0, 0, 2, 0, 0, 7, 0, 1, 9, 0, 3, 0, 0,
      11, 8, 1, 0, 10, 5, 0);

   function Hash (S : String) return Natural is
      F : constant Natural := S'First - 1;
      L : constant Natural := S'Length;
      F1, F2 : Natural := 0;
      J : Natural;
   begin
      for K in P'Range loop
         exit when L < P (K);
         J  := Character'Pos (S (P (K) + F));
         F1 := (F1 + Natural (T1 (K)) * J) mod 29;
         F2 := (F2 + Natural (T2 (K)) * J) mod 29;
      end loop;
      return (Natural (G (F1)) + Natural (G (F2))) mod 14;
   end Hash;

end Natools.Static_Maps.Web.Comments.Item_Conditions;
