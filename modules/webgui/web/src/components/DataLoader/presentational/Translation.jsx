import React from 'react';
import PropTypes from 'prop-types';
import ReactSelect from 'react-select';
import 'react-select/dist/react-select.css';
import Label from '../../common/Label/Label';
import Select from '../../common/Input/Select/Select';
import Row from '../../common/Row/Row';
import styles from './Translation.scss';
import { KEY_SPICE_TRANSLATION } from '../constants';
import MultiInputs from './MultiInputs';
import RadioButtons from '../../common/Input/RadioButtons/RadioButtons';

const Translation = (props) => {
  const { translationType,
    onSetTranslation,
    onTranslationTypeChange,
    onSetTranslationTarget,
    translationPos,
    target } = props;
  const spiceOptions = 'SUN EARTH'.split(' ').map(v => ({ value: v, label: v }));

  return (
    <Row>
      <div><RadioButtons label='Translation type'
        options={['Static', 'Spice']}
        defaultOption='Static'
        isHorizontal={false}
        onChange={(target) => onTranslationTypeChange(target)} /></div>
      <div className={styles.wrapper}>
        {translationType === KEY_SPICE_TRANSLATION
          ?
          <div className={styles.translation}>
            <Label size='medium'>Translate Data: </Label>
            <Select label={target}
              options={spiceOptions}
              onChange={onSetTranslationTarget}
              placeholder=''
            />
          </div>
          :
          <div className={styles.translation}>
            <MultiInputs presentationLabel='Translate Data'
              options={translationPos}
              onChange={onSetTranslation} />
          </div>}
      </div>
    </Row>
  );
}

Translation.propTypes = {
  translationType: PropTypes.string.isRequired,
  translationPos: PropTypes.object.isRequired,
  onTranslationTypeChange: PropTypes.func.isRequired,
  onSetTranslation: PropTypes.func.isRequired,
  onSetTranslationTarget: PropTypes.func.isRequired,
  target: PropTypes.string.isRequired
};

Translation.defaultProps = {
  translationType: 'StaticTranslation',
  translationPos: { x: 0, y: 0, z: 0 },
  onSetTranslation: () => { },
  onTranslationTypeChange: () => { },
  onSetTranslationTarget: () => { },
  target: 'Sun',
};

export default Translation;