with Interfaces; use Interfaces;

package body Natools.Static_Maps.Web.List_Templates.Commands is

   P : constant array (0 .. 5) of Natural :=
     (3, 7, 8, 9, 10, 14);

   T1 : constant array (0 .. 5) of Unsigned_8 :=
     (14, 1, 24, 33, 1, 38);

   T2 : constant array (0 .. 5) of Unsigned_8 :=
     (35, 50, 48, 31, 2, 27);

   G : constant array (0 .. 56) of Unsigned_8 :=
     (0, 26, 22, 0, 0, 20, 0, 0, 0, 22, 4, 0, 17, 0, 10, 0, 18, 0, 0, 12, 0,
      0, 16, 16, 0, 0, 0, 3, 0, 0, 0, 1, 7, 0, 0, 0, 25, 0, 0, 12, 0, 8, 14,
      2, 24, 9, 19, 24, 0, 8, 13, 0, 0, 18, 0, 0, 13);

   function Hash (S : String) return Natural is
      F : constant Natural := S'First - 1;
      L : constant Natural := S'Length;
      F1, F2 : Natural := 0;
      J : Natural;
   begin
      for K in P'Range loop
         exit when L < P (K);
         J  := Character'Pos (S (P (K) + F));
         F1 := (F1 + Natural (T1 (K)) * J) mod 57;
         F2 := (F2 + Natural (T2 (K)) * J) mod 57;
      end loop;
      return (Natural (G (F1)) + Natural (G (F2))) mod 28;
   end Hash;

end Natools.Static_Maps.Web.List_Templates.Commands;
