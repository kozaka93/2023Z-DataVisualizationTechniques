package pl.edu.pw.mini.zpoif.task4a.szybowiec.kabina.mechanizm.hamulec;

import pl.edu.pw.mini.zpoif.task4a.szybowiec.mechanizm.PlytaHamulcaAerodynamicznego;

public class HamulecAerodynamiczny extends Hamulec {

	private boolean blokada;

	private PlytaHamulcaAerodynamicznego lewaGorna = new PlytaHamulcaAerodynamicznego();
	private PlytaHamulcaAerodynamicznego lewaDolna = new PlytaHamulcaAerodynamicznego();
	private PlytaHamulcaAerodynamicznego prawaGorna = new PlytaHamulcaAerodynamicznego();
	private PlytaHamulcaAerodynamicznego prawaDolna = new PlytaHamulcaAerodynamicznego();

	@Override
	protected void hamuj(int procent) {
		otworz(procent);
	}

	@Override
	protected void odhamuj(int procent) {
		zamknij(procent);
	}

	private void otworz(int procent) {
		//wysuwa plyty hamujace
		lewaGorna.setStopienWysuniecia(procent);
		lewaDolna.setStopienWysuniecia(procent);
		prawaDolna.setStopienWysuniecia(procent);
		prawaGorna.setStopienWysuniecia(procent);
		
		if(procent > 0) {
			blokada = false;
		}
		
	}

	private void zamknij(int procent) {
		//chowa plyty hamujace
		
		lewaGorna.setStopienWysuniecia(procent);
		lewaDolna.setStopienWysuniecia(procent);
		prawaDolna.setStopienWysuniecia(procent);
		prawaGorna.setStopienWysuniecia(procent);
		
		if(procent == 0) {
			blokada = true;
		}
		
	}

}
