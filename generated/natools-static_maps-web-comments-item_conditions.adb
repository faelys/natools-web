with Interfaces; use Interfaces;

package body Natools.Static_Maps.Web.Comments.Item_Conditions is

   P : constant array (0 .. 3) of Natural :=
     (1, 2, 16, 18);

   T1 : constant array (0 .. 3) of Unsigned_8 :=
     (29, 1, 5, 3);

   T2 : constant array (0 .. 3) of Unsigned_8 :=
     (27, 19, 14, 9);

   G : constant array (0 .. 34) of Unsigned_8 :=
     (16, 0, 0, 7, 0, 0, 0, 0, 15, 0, 0, 0, 0, 0, 14, 0, 0, 12, 0, 0, 0, 0,
      3, 10, 2, 14, 3, 1, 2, 0, 0, 6, 9, 16, 11);

   function Hash (S : String) return Natural is
      F : constant Natural := S'First - 1;
      L : constant Natural := S'Length;
      F1, F2 : Natural := 0;
      J : Natural;
   begin
      for K in P'Range loop
         exit when L < P (K);
         J  := Character'Pos (S (P (K) + F));
         F1 := (F1 + Natural (T1 (K)) * J) mod 35;
         F2 := (F2 + Natural (T2 (K)) * J) mod 35;
      end loop;
      return (Natural (G (F1)) + Natural (G (F2))) mod 17;
   end Hash;

end Natools.Static_Maps.Web.Comments.Item_Conditions;
