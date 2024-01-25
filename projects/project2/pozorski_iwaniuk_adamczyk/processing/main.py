import time
import logging

from setup import parse, Config

if __name__ == "__main__":
    t0 = time.time()

    t1 = time.time()
    parse()
    logging.info(f"Parsed args, took {time.time() - t1:.2f} seconds")

    if Config.get("preprocess") == 1:
        t1 = time.time()
        from cleaning import clean

        clean()
        logging.info(f"Cleaned messages, took {time.time() - t1:.2f} seconds")

    t1 = time.time()
    from query_manager import query

    query()
    logging.info(f"Executed all queries, took {time.time() - t1:.2f} seconds")

    logging.info(f"All tasks finished, took {time.time() - t0:.2f} seconds")
