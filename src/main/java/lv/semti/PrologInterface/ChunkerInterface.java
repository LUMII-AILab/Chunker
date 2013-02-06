/*******************************************************************************
 * Copyright 2008, 2009 Institute of Mathematics and Computer Science, University of Latvia; 
 * Author: Ilmārs Poikāns
 *     This program is free software: you can redistribute it and/or modify
 *     it under the terms of the GNU General Public License as published by
 *     the Free Software Foundation, either version 3 of the License, or
 *     (at your option) any later version.
 * 
 *     This program is distributed in the hope that it will be useful,
 *     but WITHOUT ANY WARRANTY; without even the implied warranty of
 *     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *     GNU General Public License for more details.
 * 
 *     You should have received a copy of the GNU General Public License
 *     along with this program.  If not, see <http://www.gnu.org/licenses/>.
 *******************************************************************************/
package lv.semti.PrologInterface;

import java.io.File;
import java.util.ArrayList;
import java.util.Hashtable;
import java.util.List;

import jpl.Atom;
import jpl.Compound;
import jpl.JPL;
import jpl.PrologException;
import jpl.Query;
import jpl.Term;
import jpl.Util;
import jpl.Variable;
import lv.semti.morphology.analyzer.Word;

@SuppressWarnings("unchecked")
public class ChunkerInterface {
	
	public ChunkerInterface(String čunkerfolderis) throws UnsatisfiedLinkError, Exception {
		String [] args = {"pl","-g","true","-nosignals","-q"};		
		JPL.setDefaultInitArgs(args);
		
		// Ieimportējam čunkera prologa failus
        String filename = čunkerfolderis + File.separator + "config.pl";        
        Query load = new Query("consult", new Term[] { new Atom(filename) });
        if ( !load.hasSolution() )
			throw new Exception("consult('" + filename + "') failed");

        // Uzsetojam parametrus čunkerim
        Term runtime = new Atom("runtime");
        Term key = new Atom("parse_word_analyzer_enable_diminutive");        
        Term value = new Atom("true");
        Query query = new Query("set_config", new Term[] { runtime, key, value });
        //Hashtable<String,Term> solution = 
        	query.oneSolution();
        //FIXME - te arī jāčeko vai izdodas bez erroriems
       
        key = new Atom("parse_use_word_analyzer");
        value = new Atom("false");
        query = new Query("set_config", new Term[] { runtime, key, value });
        //solution = 
        	query.oneSolution();		
        
        filename = čunkerfolderis + File.separator + "parser.pl";        
		load = new Query("consult", new Term[] { new Atom(filename) });
        if ( !load.hasSolution() )
			throw new Exception("consult('" + filename + "') failed");

        filename = čunkerfolderis + File.separator + "semti-javai.pl";        
		load = new Query("consult", new Term[] { new Atom(filename) });
        if ( !load.hasSolution() )
			throw new Exception("consult('" + filename + "') failed");

        filename = čunkerfolderis + File.separator + "semti-html.pl";        
		load = new Query("consult", new Term[] { new Atom(filename) });
        if ( !load.hasSolution() )
			throw new Exception("consult('" + filename + "') failed");
	}
		
	public static Term getTextTerm(String text) {

		text = text.replaceAll("\n", "");

		return new Atom(text);
	}
	
	public static Term[] getTokenTerms(String[] tokens) {

		Term[] terms = new Term[tokens.length];
		
		for (int i = 0; i < tokens.length; i++)
			terms[i] = getTextTerm(tokens[i]);

		return terms;
	}
	
	public ArrayList<ChunkerVariant> parse(String sentence) {
		List<Word> words = PrologWrapper.tokenize(sentence);
		List<String> tokens = PrologWrapper.words2tokens(words);
		return parseTokens(tokens);
	}

	public ArrayList<ChunkerVariant> parseTokens(List<String> tokens) {
		return parseTokens(tokens.toArray(new String[tokens.size()]));
	}

	public ArrayList<ChunkerVariant> parseTokens(String[] tokens) {
		
		Term sentence = Util.termArrayToList(getTokenTerms(tokens));
	    Query query = new Query("parse_by_config", new Term[] { sentence, new Variable("OUT") });
        Hashtable<String,Term> solution = query.oneSolution();
        Term answer = solution.get("OUT");
	    
        if (answer.isVariable()) {
        	//System.out.printf("Nesačunkojās '%s', atbilde: '%s'\n", čunks.toString(), answer.toString());
        	System.out.printf("Chunking failed with answer '%s'\n", answer.toString());
        	return new ArrayList<ChunkerVariant>();
        } 

	    ArrayList<ChunkerVariant> rezults = apstaigātAtbildi(answer);
	    return rezults;        	
	}

	public ArrayList<ChunkerVariant> parseWords(List<Word> words) {
		
		// TODO: modify A table in Prolog from word data here and let prolog know, that A table is prepared
		
		List<String> tokens = PrologWrapper.words2tokens(words);
		return parseTokens(tokens);
	}

	
	private ArrayList<ChunkerVariant> apstaigātAtbildi (Term atbilde)
	{    
		ArrayList<ChunkerVariant> results = null;
		if (atbilde.isCompound())
		{
			Compound c = (Compound) atbilde;
			if (c.name().equals("."))
			{
				if (c.arity() != 2)
					throw new RuntimeException("unknown list compound: \n" + c);

				Term first = c.arg(1);
				Term second = c.arg(2);				
					    
			    // variantu pārveido uz kastīšu html
				Query query = new Query("chunks_html", new Term[] { first, new Variable("OUT") });
				Hashtable<String,Term> solution = query.oneSolution();
			    Term htmlterm = solution.get("OUT");
			    String html = htmlterm.toString().replaceAll("\\\\n", "\n");

			    ChunkerVariant pirmaisČunkotājaVariants = new ChunkerVariant(first, html);
			    
				results = new ArrayList<ChunkerVariant>();				
				results.add(pirmaisČunkotājaVariants);
				
				ArrayList<ChunkerVariant> pārējieVarianti = apstaigātAtbildi(second);
				if (pārējieVarianti!=null) 
					for (ChunkerVariant variants : pārējieVarianti) {
						results.add(variants);
					}
			}
		}
		else throw new RuntimeException("Atbilde " + atbilde.toString() + " nav compounds");
		
		return results;
	}
	
	public void izdzēstAtabulu() {
        // izdzēšam prolog agrāk pieliktos vārdus
	    Query query = new Query("retracttool");
	    //Hashtable<String,Term> solution = 
	    	query.oneSolution();
	    //FIXME - būtu jāčeko vai izdodas - jo ja ieejā būs nekorekti dati, tad savādāk nekas nepamanīsies
	}

	public void pieliktVārdu(String prolog) throws Exception{
		//pieņemam, ka parametrā ir jauks prologa formāta String kas satur vārda morfologjisko analīzi
		try {
			Term analīze = new Atom(prolog);
		    Query query = new Query("add_analyzer_words", new Term[] { analīze });
		    //Hashtable<String,Term> solution = 
		    	query.oneSolution();
		} catch (PrologException e) {
			throw new Exception("Nevarējām pielikt prologvārdu "+prolog);
		}
	    //FIXME - būtu jāčeko vai izdodas - jo ja ieejā būs nekorekti dati, tad savādāk nekas nepamanīsies
	}
	
	public void setMaxChunkLength(int i) {
        Term runtime = new Atom("runtime");
        jpl.Integer j = new jpl.Integer(i);
        Atom key = new Atom("max_chunk_length");
        Query query = new Query("set_config", new Term[] { runtime, key, j });
        //Hashtable<String,Term> solution = 
        	query.oneSolution();
	}

	public void setParseTimeLimit(int i) {
        Term runtime = new Atom("runtime");
        jpl.Integer j = new jpl.Integer(i);
        Atom key = new Atom("parse_time_limit");
        Query query = new Query("set_config", new Term[] { runtime, key, j });
        //Hashtable<String,Term> solution = 
        	query.oneSolution();
	}
}
