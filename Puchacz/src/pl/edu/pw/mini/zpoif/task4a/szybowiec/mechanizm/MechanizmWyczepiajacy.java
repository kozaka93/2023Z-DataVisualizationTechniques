package pl.edu.pw.mini.zpoif.task4a.szybowiec.mechanizm;

public class MechanizmWyczepiajacy extends MechanizmSterowanyCiegnem {

	private static final int MAKSYMALNE_WYZWALAJACE_POLOZENIE_UCHWYTU_DO_WYCZEPIENIA = 75;
	
	private int wyzwalajacePolozenieUchwytuDoWyczepienia = MAKSYMALNE_WYZWALAJACE_POLOZENIE_UCHWYTU_DO_WYCZEPIENIA;
	
	private boolean wyczepiony;

	@Override
	public void wybierz(int procentWybrania) {
		super.wybierz(procentWybrania);
		if(getPolozenieUchwytu() > wyzwalajacePolozenieUchwytuDoWyczepienia) {
			wyczepiony = true;
			System.out.println();
		}
	}

}
