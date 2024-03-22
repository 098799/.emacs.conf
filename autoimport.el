(defvar my-symbol-map
  '(
    ("os" . "import os")
    ("BytesIO" . "from io import BytesIO")
    ("Summary" . "from ontology_service.annotation_summary.models import Summary")
    ("ExportService" . "from ontology_service.annotation_summary.services.export_service import ExportService")
    ("cached_property" . "from functools import cached_property")
    ("defaultdict" . "from collections import defaultdict")
    ("uuid4" . "from uuid import uuid4")
    ("AcpTestSet" . "from pythia_service.kdp.models import AcpTestSet")
    ("AcpTestCase" . "from pythia_service.kdp.models import AcpTestCase")
    ("Provision" . "from pythia_service.ontology.models import Provision")
    ("setup_django" . "import os
os.environ['SENTRY_IS_ENABLED'] = 'False'
from service_common.util.framework_util import setup_django
setup_django('pythia_service', force=True, load_env=True)")
    ("Text" . "from pythia_service.document.models import Text")
    ("Segment" . "from pythia_service.document.models import Segment")
    ("Document" . "from pythia_service.document.models import Document")
    ("px" . "import plotly.express as px")
    ("glob" . "from glob import glob")
    ("json" . "import json")
    ("requests" . "import requests")
    ("pytest" . "import pytest")
    ("tqdm" . "from tqdm import tqdm")
    ("Path" . "from pathlib import Path")
    ("partial" . "from functools import partial")
    ("cache" . "from functools import cache")
    ("count" . "from itertools import count")
    ("profile" . "from memory_profiler import profile")
    ("load_dotenv" . "from dotenv import load_dotenv")
    ("st" . "import streamlit as st")
    ("re" . "import re")
    ("pd" . "import pandas as pd")
    ("np" . "import numpy as np")
    ("torch" . "import torch")
    ("Q" . "from django.db.models import Q")
    ("Query" . "from fastapi import Query")
    ("BaseModel" . "from pydantic import BaseModel")
    ("Text" . "from pythia_service.document.models import Text")
    ("AutoModel" . "from transformers import AutoModel")
    ("AutoModelForQuestionAnswering" . "AutoModelForQuestionAnswering")
    ("AutoModelForSeq2SeqLM" . "from transformers import AutoModelForSeq2SeqLM")
    ("AutoTokenizer" . "from transformers import AutoTokenizer")
    ("T5ForConditionalGeneration" . "from transformers import T5ForConditionalGeneration")
    ("pipeline" . "from transformers import pipeline")
    ("Provision" . "from pythia_service.ontology.models import Provision")
    )
  )

(defun autoimport ()
  "Auto-import a symbol at point by finding the suitable place to insert the import statement in a Python buffer.
If no existing 'import' or 'from' statement is found, insert at the top of the file."
  (interactive)
  (let* ((sym (thing-at-point 'symbol))
         (import-stmt (cdr (assoc sym my-symbol-map))))
    (when import-stmt
      (save-excursion
        (goto-char (point-max)) ; Start from the end of the buffer
        ;; Search backward for the import or from patterns. If not found, go to the buffer's start.
        (unless (re-search-backward "^\\(import \\|from \\)" nil t)
          (goto-char (point-min)))
        ;; If found, move to the next line; if not found, we're already at the top.
        (when (looking-at "^\\(import \\|from \\)")
          (forward-line 1))
        ;; Insert the import statement.
        (insert import-stmt "\n")))))

(defun autoimport ()
  "Auto-import a symbol at point by finding the suitable place to insert the import statement in a Python buffer.
If no existing 'import' or 'from' statement is found below the current point, insert at the beginning of the buffer."
  (interactive)
  (let* ((sym (thing-at-point 'symbol))
         (import-stmt (cdr (assoc sym my-symbol-map))))
    (when import-stmt
      (save-excursion
        (let ((current-pos (point)))
          (goto-char current-pos) ; Start from the current cursor position
          ;; Search forward for the import or from patterns.
          (if (re-search-forward "^\\(import \\|from \\)" nil t)
              (progn
                ;; If found, move to the previous line.
                (forward-line -1)
                ;; Insert the import statement.
                (insert import-stmt "\n"))
            ;; If not found, go to the beginning of the buffer and insert the import statement.
            (goto-char (point-min))
            (insert import-stmt "\n")))))))
