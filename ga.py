import logging, random, sys
from collections import namedtuple

logger = logging.getLogger(__name__)
logger.setLevel(logging.DEBUG)
if not len(logger.handlers):
  clogger = logging.StreamHandler(sys.stdout)
  clogger.setLevel(logging.DEBUG)
  #clogger.setFormatter(logging.Formatter('%(message)s'))
  logger.addHandler(clogger)

GENES_TO_ENCODING = {
  '0':  0x0,
  '1':  0x1,
  '2':  0x2,
  '3':  0x3,
  '4':  0x4,
  '5':  0x5,
  '6':  0x6,
  '7':  0x7,
  '8':  0x8,
  '9':  0x9,
  '+':  0xa,
  '-':  0xb,
  '*':  0xc,
  '/':  0xd,
}

ENCODING_TO_GENES = {v:k for k,v in GENES_TO_ENCODING.items()}
GENE_LENGTH = 4 # 4 bits - how we chose to represent each gene
CHROMOSOME_LENGTH = 5 # 5 genes per chromosome

def extract(chromosome):
  genes = []
  m = 2**GENE_LENGTH - 1 # what we'll use to mask our genes
  for i in range(CHROMOSOME_LENGTH):
    # we want to extract each group of GENE_LENGTH bits from the chromosone
    mask = m << i*GENE_LENGTH
    gene = (chromosome & mask) >> i*GENE_LENGTH
    genes.append(gene)
  genes.reverse() # as we went from right to left
  return genes

assert extract(0x43ae2) == [4,3,10,14,2] 

def validate(genes):
  # we assume num, op, num, op, num
  m = False
  for gene in genes:
    if m:
      if gene not in ['+','-','*','/']:
        return False
      m = False
    else:
      if gene not in ['0','1','2','3','4','5','6','7','8','9','0']:
        return False
      m = True
  return True # we have a valid sequence

assert validate(['+']) == False
assert validate(['0','/','3','+','1']) == True

def decode(chromosome):
  # there's a chance the gene encoding does not match anything we know, like 0xf
  genes = extract(chromosome)
  return [ENCODING_TO_GENES.get(gene, None) for gene in genes]
  
def evaluate(chromosome, target):
  values = decode(chromosome)
  # what about things like ++2 or 2/None3?
  if validate(values):
    try:
      r = eval(''.join(values))
      return fitness(r, target)
    except ZeroDivisionError as e: # any other exception, we'll want to see
      return float('-inf')
  else:
    # invalid chromosome
    return float('-inf')

def fitness(solution, target):
  return 1/(target-solution)

def create_initial_population(population_size):
  chromosomes = []
  for i in range(population_size):
    # we generate each gene separately
    chromosome = 0
    # there's no point generating invalid chromosomes as it means they'll be discarded straight away
    while not validate(decode(chromosome)):
      chromosome = 0
      for g in range(CHROMOSOME_LENGTH):
        gene = random.randint(0,2**GENE_LENGTH-1) # so GENE_LENGTH bits
        chromosome ^= gene << g*GENE_LENGTH
    chromosomes.append( chromosome )
  return chromosomes

assert len(create_initial_population(20)) == 20

#TODO can this take a number of weighted choices instead?
def weighted_random_choice(chromosomes):
  total_fitness = sum(c.fitness for c in chromosomes)
  pick = random.uniform(0, total_fitness)
  curr = 0
  for c in chromosomes:
    curr += c.fitness
    if curr >= pick:
      return c
  raise Exception('that should never happen! {0} {1}'.format(total_fitness, pick))

def crossover(a, b, gene_number):
  # we're counting genes from left to right
  mask_swapped = 2**(GENE_LENGTH*(CHROMOSOME_LENGTH-gene_number))-1
  mask_notswapped = ~mask_swapped & 2**(CHROMOSOME_LENGTH*GENE_LENGTH)-1

  new_a = (a & mask_notswapped) ^ (b & mask_swapped)
  new_b = (b & mask_notswapped) ^ (a & mask_swapped)
  #print(hex(new_a),hex(new_b))
  return (new_a, new_b)

assert crossover(0x10001,0x01010,3) == (0x10010,0x01001)
assert crossover(0x54321,0x12345,4) == (0x54325,0x12341)
assert crossover(0x54321,0x12345,3) == (0x54345,0x12321)

def mutate(chromosome, probabilities, mutation_rate):
  # probabilities is an array of uniform(0,1) probabilities
  for idx, p in enumerate(probabilities):
    if p < mutation_rate:
      mask = 1 << (CHROMOSOME_LENGTH*GENE_LENGTH-idx-1) # ugly - maybe we should always index from the right
      chromosome ^= mask
  #print(bin(chromosome))
  return chromosome

assert mutate(0x30100,[1,1,1,0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,0],0.5) == 0x20101

Chromosome = namedtuple('Chromosome', 'repr fitness')

def run(target=42, pop_size=500, max_rounds=2000, crossover_rate=0.7, mutation_rate=0.1):
  pop = create_initial_population(pop_size)
  chromosomes = []
  new_pop = []
  for round_number in range(max_rounds):
    if round_number % 100 == 0:
      logger.info('round {0}'.format(round_number))
    #1 evaluate fitness for all chromosomes in the current population
    for chromosome in pop:
      try:
        #print(decode(chromosome))
        fitness_score = evaluate(chromosome, target)
      except Exception:
        # result found! this is due to how our fitness function works
        logger.info('we have a winner! {0}'.format(chromosome))
      if fitness_score > 0: # we only look at positive fitness - otherwise weighted_random_choice won't work
        chromosomes.append( Chromosome(chromosome, fitness_score) )
      # otherwise it *dies*!

    #logger.info('fitness found for {0} chromosomes'.format(len(chromosomes)))

    # we now populate our next generation
    while len(new_pop) < pop_size:
      #2 select 2 members at random, but based on their fitness score
      a = weighted_random_choice(chromosomes).repr
      #TODO we're doing this with replacement - is that right?
      b = weighted_random_choice(chromosomes).repr
      
      #3 crossover?
      if random.random() < crossover_rate:
        # pick a random gene
        gene_number = random.randint(0,CHROMOSOME_LENGTH-1)
        a, b = crossover(a,b, gene_number)
      #4 mutation
      probs = [random.uniform(0,1) for _ in range(CHROMOSOME_LENGTH*GENE_LENGTH)]
      a = mutate(a, probs, mutation_rate)
      probs = [random.uniform(0,1) for _ in range(CHROMOSOME_LENGTH*GENE_LENGTH)]
      b = mutate(b, probs, mutation_rate)
      new_pop.append(a)
      new_pop.append(b)
    pop = new_pop
  # if we reach here, no solution was found!
  leftovers = []
  for chromosome in pop:
    fitness_score = evaluate(chromosome, target)
    leftovers.append((fitness_score,chromosome))
  for fitness_score, chromosome in sorted(leftovers, key=lambda x: x[0]): 
    if validate(decode(chromosome)) and fitness_score > 0.1:
      logger.info('c: {0} s: {1}'.format(''.join(decode(chromosome)), fitness_score))

def foo():
  logger.info('wah')
